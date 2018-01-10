-- Copyright (C) 2017 Red Hat, Inc.
--
-- This file is part of bdcs-api.
--
-- bdcs-api is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-api is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-api.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}

module ServerSpec
  where

import           BDCS.API.Recipe(Recipe(..), RecipeModule(..))
import           BDCS.API.Recipes(CommitDetails(..), RecipeDiffType(..), RecipeDiffEntry(..))
import           BDCS.API.Server
import           BDCS.API.V0
import           Control.Conditional(whenM)
import           Control.Exception(throwIO)
import           Control.Monad.Loops(allM)
import           Data.List(isSuffixOf)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           System.Directory(copyFileWithMetadata, createDirectoryIfMissing, doesPathExist,
                                  listDirectory, removeDirectoryRecursive)
import           System.FilePath.Posix((</>))
import           Text.Printf(printf)
import           Test.Hspec

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Use null"  :: String) #-}
{-# ANN module ("HLint: ignore Use head"  :: String) #-}

-- Client API
getStatus :: ClientM ServerStatus
getPackage :: T.Text -> ClientM PackageInfo
getProjectsDepsolve :: String -> ClientM ProjectsDepsolveResponse
getErr :: ClientM [T.Text]
getRecipes :: Maybe Int -> Maybe Int -> ClientM RecipesListResponse
getRecipesInfo :: String -> ClientM RecipesInfoResponse
getRecipesChanges :: String -> Maybe Int -> Maybe Int -> ClientM RecipesChangesResponse
postRecipesNew :: Recipe -> ClientM RecipesStatusResponse
deleteRecipes :: String -> ClientM RecipesStatusResponse
postRecipesUndo :: String -> String -> ClientM RecipesStatusResponse
postRecipesWorkspace :: Recipe -> ClientM RecipesStatusResponse
postRecipesTag :: String -> ClientM RecipesStatusResponse
getRecipesDiff :: String -> String -> String -> ClientM RecipesDiffResponse
getRecipesDepsolve :: String -> ClientM RecipesDepsolveResponse
getRecipesFreeze :: String -> ClientM RecipesFreezeResponse
getStatus :<|> getPackage :<|> getProjectsDepsolve :<|> getErr
          :<|> getRecipes :<|> getRecipesInfo :<|> getRecipesChanges
          :<|> postRecipesNew :<|> deleteRecipes :<|> postRecipesUndo
          :<|> postRecipesWorkspace :<|> postRecipesTag :<|> getRecipesDiff
          :<|> getRecipesDepsolve :<|> getRecipesFreeze = client proxyAPI


-- Test results, depends on the contents of the ./tests/recipes files.
recipesListResponse1 :: RecipesListResponse
recipesListResponse1 = RecipesListResponse ["glusterfs", "http-server", "kubernetes", "test-fake"] 0 20 4

recipesListResponse2 :: RecipesListResponse
recipesListResponse2 = RecipesListResponse ["http-server"] 1 1 4

missingRecipeResponse :: RecipesInfoResponse
missingRecipeResponse = RecipesInfoResponse [] [] [RecipesAPIError "missing-recipe" "missing-recipe.toml is not present on branch master"]

httpserverRecipeResponse :: RecipesInfoResponse
httpserverRecipeResponse =
    RecipesInfoResponse [WorkspaceChanges "http-server" False]
                        [Recipe "http-server" (Just "0.2.0") "An example http server with PHP and MySQL support."
                          [RecipeModule "tmux" "2.2",
                           RecipeModule "openssh-server" "6.6.*",
                           RecipeModule "rsync" "3.0.*"]
                          [RecipeModule "httpd" "2.4.*",
                           RecipeModule "mod_auth_kerb" "5.4",
                           RecipeModule "mod_ssl" "2.4.*",
                           RecipeModule "php" "5.4.*",
                           RecipeModule "php-mysql" "5.4.*"]
                        ]
                        []

multipleRecipeResponse :: RecipesInfoResponse
multipleRecipeResponse =
    RecipesInfoResponse [WorkspaceChanges "glusterfs" False,
                         WorkspaceChanges "http-server" False]
                        [Recipe "glusterfs" (Just "0.0.1") "An example GlusterFS server with samba"
                          [RecipeModule "samba" "4.2.*"]
                          [RecipeModule "glusterfs" "3.7.*",
                           RecipeModule "glusterfs-cli" "3.7.*"],
                         Recipe "http-server" (Just "0.2.0") "An example http server with PHP and MySQL support."
                          [RecipeModule "tmux" "2.2",
                           RecipeModule "openssh-server" "6.6.*",
                           RecipeModule "rsync" "3.0.*"]
                          [RecipeModule "httpd" "2.4.*",
                           RecipeModule "mod_auth_kerb" "5.4",
                           RecipeModule "mod_ssl" "2.4.*",
                           RecipeModule "php" "5.4.*",
                           RecipeModule "php-mysql" "5.4.*"]
                        ]
                        []

errorRecipeResponse :: RecipesInfoResponse
errorRecipeResponse =
    RecipesInfoResponse [WorkspaceChanges "glusterfs" False,
                         WorkspaceChanges "http-server" False]
                        [Recipe "glusterfs" (Just "0.0.1") "An example GlusterFS server with samba"
                          [RecipeModule "samba" "4.2.*"]
                          [RecipeModule "glusterfs" "3.7.*",
                           RecipeModule "glusterfs-cli" "3.7.*"],
                         Recipe "http-server" (Just "0.2.0") "An example http server with PHP and MySQL support."
                          [RecipeModule "tmux" "2.2",
                           RecipeModule "openssh-server" "6.6.*",
                           RecipeModule "rsync" "3.0.*"]
                          [RecipeModule "httpd" "2.4.*",
                           RecipeModule "mod_auth_kerb" "5.4",
                           RecipeModule "mod_ssl" "2.4.*",
                           RecipeModule "php" "5.4.*",
                           RecipeModule "php-mysql" "5.4.*"]
                        ]
                        [RecipesAPIError "missing-recipe" "missing-recipe.toml is not present on branch master"]

recipesNewResponse :: RecipesStatusResponse
recipesNewResponse = RecipesStatusResponse True []

aTestRecipe :: Recipe
aTestRecipe = Recipe "A Test Recipe" (Just "0.0.1") "A simple recipe to use for testing"
                     [RecipeModule "rsync" "3.0.*"]
                     [RecipeModule "httpd" "2.4.*"]

recipesDepsolveResponse1 :: RecipesDepsolveResponse
recipesDepsolveResponse1 =
    RecipesDepsolveResponse [RecipeDependencies (Recipe "test-fake" (Just "0.0.1")  "A test recipe that uses the fake rpms"
                                                  [RecipeModule "bdcs-fake-lisa""1.0.*"]
                                                  [RecipeModule "bdcs-fake-bart" "1.3.*"])
                                                [PackageNEVRA "bdcs-fake-bart" (Just "0") "1.3.1" "12" "x86_64",
                                                 PackageNEVRA "bdcs-fake-homer" (Just "0") "2.0.1" "4" "x86_64",
                                                 PackageNEVRA "bdcs-fake-lisa" (Just "3") "1.0.0" "1" "x86_64",
                                                 PackageNEVRA "bdcs-fake-sax" (Just "0") "3.8.1" "1" "x86_64"]
                                                [PackageNEVRA "bdcs-fake-bart" (Just "0") "1.3.1" "12" "x86_64",
                                                 PackageNEVRA "bdcs-fake-lisa" (Just "3") "1.0.0" "1" "x86_64"]]
                            []

recipesDepsolveResponse2 :: RecipesDepsolveResponse
recipesDepsolveResponse2 =
    RecipesDepsolveResponse [] [RecipesAPIError "unknown-recipe" "unknown-recipe.toml is not present on branch master"]

recipesFreezeResponse1 :: RecipesFreezeResponse
recipesFreezeResponse1 =
    RecipesFreezeResponse [Recipe "test-fake" (Just "0.0.1") "A test recipe that uses the fake rpms"
                            [RecipeModule "bdcs-fake-lisa" "3:1.0.0-1"]
                            [RecipeModule "bdcs-fake-bart" "1.3.1-12"]]
                          []

recipesFreezeResponse2 :: RecipesFreezeResponse
recipesFreezeResponse2 =
    RecipesFreezeResponse [] [RecipesAPIError "unknown-recipe" "unknown-recipe.toml is not present on branch master"]

projectsDepsolveResponse1 :: ProjectsDepsolveResponse
projectsDepsolveResponse1 =
    ProjectsDepsolveResponse [PackageNEVRA "bdcs-fake-lisa" (Just "3") "1.0.0" "1" "x86_64",
                              PackageNEVRA "bdcs-fake-sax" (Just "0") "3.8.1" "1" "x86_64"]

projectsDepsolveResponse2 :: ProjectsDepsolveResponse
projectsDepsolveResponse2 =
    ProjectsDepsolveResponse []

-- Post 10 changes to the test recipe
postMultipleChanges :: ClientM Bool
postMultipleChanges = allM newVersion [0..10]
  where
    newVersion :: Integer -> ClientM Bool
    newVersion patch = status_ok <$> postRecipesNew aTestRecipe {rVersion = Just $ patchedVersion patch}

    patchedVersion :: Integer -> String
    patchedVersion = printf "0.1.%d"

    status_ok :: RecipesStatusResponse -> Bool
    status_ok = rsrStatus

-- If it has 0 errors, 1 change named http-server, 0 offset and a limit of 20 it passes
recipesChangesTest1 :: ClientM Bool
recipesChangesTest1 = do
    response <- getRecipesChanges "http-server" Nothing Nothing
    return $ length_ok response && name_ok response && error_ok response && limits_ok response
  where
    length_ok :: RecipesChangesResponse -> Bool
    length_ok response = length (rcrRecipes response) == 1

    name_ok :: RecipesChangesResponse -> Bool
    name_ok response = rcName (rcrRecipes response !! 0) == "http-server"

    error_ok :: RecipesChangesResponse -> Bool
    error_ok response = length (rcrErrors response) == 0

    limits_ok :: RecipesChangesResponse -> Bool
    limits_ok response = rcrOffset response == 0 && rcrLimit response == 20

-- If it has 0 errors, 1 change named http-server, 1 named glusterfs, 0 offset and a limit of 20 it passes
recipesChangesTest2 :: ClientM Bool
recipesChangesTest2 = do
    response <- getRecipesChanges "http-server,glusterfs" Nothing Nothing
    return $ length_ok response && name_ok response && error_ok response && limits_ok response
  where
    length_ok :: RecipesChangesResponse -> Bool
    length_ok response = length (rcrRecipes response) == 2

    name_ok :: RecipesChangesResponse -> Bool
    name_ok response = rcName (rcrRecipes response !! 0) == "glusterfs" &&
                       rcName (rcrRecipes response !! 1) == "http-server"

    error_ok :: RecipesChangesResponse -> Bool
    error_ok response = length (rcrErrors response) == 0

    limits_ok :: RecipesChangesResponse -> Bool
    limits_ok response = rcrOffset response == 0 && rcrLimit response == 20

-- | Check that limit and offset are parsed correctly
--
-- This must be called after posting > 10 changes to the recipe
recipesChangesTest3 :: ClientM Bool
recipesChangesTest3 = do
    response <- getRecipesChanges "A Test Recipe" (Just 5) (Just 5)
    return $ limits_ok response && length_ok response && name_ok response && total_ok response
  where
    limits_ok :: RecipesChangesResponse -> Bool
    limits_ok response = rcrOffset response == 5 && rcrLimit response == 5

    -- Should only be 1 recipe in the response
    length_ok :: RecipesChangesResponse -> Bool
    length_ok response = length (rcrRecipes response) == 1

    name_ok :: RecipesChangesResponse -> Bool
    name_ok response = rcName (rcrRecipes response !! 0) == "A Test Recipe"

    total_ok :: RecipesChangesResponse -> Bool
    total_ok response = rcTotal (rcrRecipes response !! 0) == 5


-- | Check that deleting a recipe removes it from the list
recipesDeleteTest :: ClientM Bool
recipesDeleteTest = do
    result_1 <- recipe_in_list
    result_2 <- delete_recipe
    result_3 <- recipe_not_in_list
    return $ result_1 && result_2 && result_3
  where
    -- Is the recipe in the list?
    recipe_in_list :: ClientM Bool
    recipe_in_list = do
        response <- getRecipes Nothing Nothing
        return $ "A-Test-Recipe" `elem` rlrRecipes response

    -- Delete it from the list
    delete_recipe :: ClientM Bool
    delete_recipe = do
        response <- deleteRecipes "A Test Recipe"
        return $ rsrStatus response

    -- Is it NOT in the list?
    recipe_not_in_list = do
        response <- getRecipes Nothing Nothing
        return $ "A-Test-Recipe" `notElem` rlrRecipes response

-- | Check reverting a recipe to a previous commit
recipesUndoTest :: ClientM Bool
recipesUndoTest = do
    -- Get a list of the commits
    response_1 <- getRecipesChanges "A Test Recipe" Nothing Nothing
    let commit = cdCommit (rcChange (rcrRecipes response_1 !! 0) !! 1)

    -- Revert to a previous commit
    rsrStatus <$> postRecipesUndo "A Test Recipe" (T.unpack commit)

-- | Check that writing to the workspace storage works
recipesWorkspaceTest :: ClientM Bool
recipesWorkspaceTest = status_ok <$> postRecipesWorkspace aTestRecipe {rDescription = "A workspace only recipe"}
  where
    status_ok :: RecipesStatusResponse -> Bool
    status_ok = rsrStatus

-- | Test the various /recipes/diff/ methods
recipesDiffTest :: ClientM Bool
recipesDiffTest = do
    result_1 <- oldest_diff
    result_2 <- workspace_diff
    result_3 <- commit_diff
    return $ result_1 && result_2 && result_3
  where
    oldest_diff = do
        response <- getRecipesChanges "A Test Recipe" Nothing Nothing
        let first_commit = cdCommit $ last $ rcChange (rcrRecipes response !! 0)

        response_1 <- getRecipesDiff "A Test Recipe" (T.unpack first_commit) "NEWEST"
        let old_version = rdtVersion $ rdeOld (rdrDiff response_1 !! 0)
        let new_version = rdtVersion $ rdeNew (rdrDiff response_1 !! 0)
        return $ old_version /= new_version && new_version == Just "0.1.9"

    workspace_diff = do
        response <- getRecipesChanges "A Test Recipe" Nothing Nothing
        let first_commit = cdCommit $ last $ rcChange (rcrRecipes response !! 0)

        response_1 <- getRecipesDiff "A Test Recipe" (T.unpack first_commit) "WORKSPACE"
        let old_desc = rdtDescription $ rdeOld (rdrDiff response_1 !! 0)
        let new_desc = rdtDescription $ rdeNew (rdrDiff response_1 !! 0)
        return $ old_desc /= new_desc && new_desc == "A workspace only recipe"

    commit_diff = do
        response <- getRecipesChanges "A Test Recipe" Nothing Nothing
        let first_commit = cdCommit $ last $ rcChange (rcrRecipes response !! 0)
        let newer_commit = cdCommit (rcChange (rcrRecipes response !! 0) !! 3)

        response_1 <- getRecipesDiff "A Test Recipe" (T.unpack first_commit) (T.unpack newer_commit)
        let old_version = rdtVersion $ rdeOld (rdrDiff response_1 !! 0)
        let new_version = rdtVersion $ rdeNew (rdrDiff response_1 !! 0)
        return $ old_version /= new_version && new_version == Just "0.1.8"


-- | Setup the temporary repo directory with some example recipes
--
-- If the directory exists it is first removed.
-- Then example recipes are copied into it
setupTempRepoDir :: FilePath -> FilePath -> IO ()
setupTempRepoDir exampleRecipes gitRepoPath = do
        whenM (doesPathExist gitRepoPath) (removeDirectoryRecursive gitRepoPath)
        createDirectoryIfMissing True gitRepoPath
        copyTOMLFiles exampleRecipes gitRepoPath
  where
    -- Copy files ending with .toml from one directory to another
    copyTOMLFiles :: FilePath -> FilePath -> IO ()
    copyTOMLFiles fromDir toDir = do
        files <- filter (".toml" `isSuffixOf`) <$> listDirectory fromDir
        let fromFiles = map (fromDir </>) files
        let toFiles = map (toDir </>) files
        mapM_ (\(from, to) -> copyFileWithMetadata from to) $ zip fromFiles toFiles


spec :: Spec
spec = do
    describe "Setup" $ do
        it "Setup the temporary test directory" $
            setupTempRepoDir "./tests/recipes/" "/var/tmp/bdcs-tmp-recipes/"

        it "Copy the test database to /var/tmp/test-bdcs.db" $
            copyFileWithMetadata "./tests/mddb/metadata.db" "/var/tmp/test-bdcs.db"

    describe "/api" $
        -- NOTE that mkApp is executed for EACH of the 'it' sections
        withClient (mkApp "/var/tmp/bdcs-tmp-recipes/" "/var/tmp/test-bdcs.db") $ do
            it "API Status" $ \env ->
                try env getStatus `shouldReturn` ServerStatus "0.0.0" "0" "0" False

            it "list the available recipes" $ \env ->
                try env (getRecipes Nothing Nothing) `shouldReturn` recipesListResponse1

            it "list recipes with offset and limit" $ \env ->
                try env (getRecipes (Just 1) (Just 1)) `shouldReturn` recipesListResponse2

            it "Get a non-existant recipe's info" $ \env ->
                try env (getRecipesInfo "missing-recipe") `shouldReturn` missingRecipeResponse

            it "Get the http-server recipe's info" $ \env ->
                try env (getRecipesInfo "http-server") `shouldReturn` httpserverRecipeResponse

            it "Get multiple recipe's info" $ \env ->
                try env (getRecipesInfo "http-server,glusterfs") `shouldReturn` multipleRecipeResponse

            it "Get http-server recipe and missing-recipe info" $ \env ->
                try env (getRecipesInfo "http-server,missing-recipe,glusterfs") `shouldReturn` errorRecipeResponse
            it "Post a test recipe" $ \env ->
                try env (postRecipesNew aTestRecipe) `shouldReturn` recipesNewResponse

            it "Post several changes to test recipe" $ \env ->
                try env postMultipleChanges `shouldReturn` True

            it "Get changes to http-server recipe" $ \env ->
                try env recipesChangesTest1 `shouldReturn` True

            it "Get changes to http-server and glusterfs recipes" $ \env ->
                try env recipesChangesTest2 `shouldReturn` True

            it "Check offset and limit usage" $ \env ->
                try env recipesChangesTest3 `shouldReturn` True

            it "Check delete recipe" $ \env ->
                try env recipesDeleteTest `shouldReturn` True

            it "Undo the recipe delete" $ \env ->
                try env recipesUndoTest `shouldReturn` True

            it "Write a recipe to the workspace" $ \env ->
                try env recipesWorkspaceTest `shouldReturn` True

            it "Tag the most recent commit of the recipe" $ \env ->
                try env (postRecipesTag "A Test Recipe") `shouldReturn` RecipesStatusResponse True []

            it "Get the recipe differences" $ \env ->
                try env recipesDiffTest `shouldReturn` True

            it "Depsolve the test-fake recipe" $ \env ->
                try env (getRecipesDepsolve "test-fake") `shouldReturn` recipesDepsolveResponse1

            it "Depsolve an unknown recipe" $ \env ->
                try env (getRecipesDepsolve "unknown-recipe") `shouldReturn` recipesDepsolveResponse2

            it "Get a frozen test-fake recipe" $ \env ->
                try env (getRecipesFreeze "test-fake") `shouldReturn` recipesFreezeResponse1

            it "Freeze an unknown recipe" $ \env ->
                try env (getRecipesFreeze "unknown-recipe") `shouldReturn` recipesFreezeResponse2

            it "Depsolve a test package" $ \env ->
                try env (getProjectsDepsolve "bdcs-fake-lisa") `shouldReturn` projectsDepsolveResponse1

            it "Depsolve an unknown package" $ \env ->
                try env (getProjectsDepsolve "unknown-recipe") `shouldReturn` projectsDepsolveResponse2

--    describe "cleanup" $
--        it "Remove the temporary directory" $
--            removeDirectoryRecursive "/var/tmp/bdcs-tmp-recipes/"


withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
    beforeAll (newManager defaultManagerSettings) $
        flip aroundWith innerSpec $ \action mgr ->
            testWithApplication x $ \port -> do
                let base = BaseUrl Http "localhost" port ""
                action (ClientEnv mgr base)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<< runClientM action clientEnv
