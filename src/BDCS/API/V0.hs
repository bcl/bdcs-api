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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK ignore-exports, prune #-}

{-| API v0 routes
-}
module BDCS.API.V0(BuildInfo(..),
               ComposeBody(..),
               ComposeDeleteResponse(..),
               ComposeFailedResponse(..),
               ComposeFinishedResponse(..),
               ComposeQueueResponse(..),
               ComposeResponse(..),
               ComposeStatusResponse(..),
               ComposeTypesResponse(..),
               Metadata(..),
               ModuleName(..),
               ModulesListResponse(..),
               PackageNEVRA(..),
               ProjectInfo(..),
               ProjectsDepsolveResponse(..),
               ProjectsInfoResponse(..),
               ProjectsListResponse(..),
               RecipesListResponse(..),
               RecipesInfoResponse(..),
               RecipesChangesResponse(..),
               RecipesDiffResponse(..),
               RecipesDepsolveResponse(..),
               RecipesFreezeResponse(..),
               RecipesStatusResponse(..),
               RecipesAPIError(..),
               RecipeChanges(..),
               RecipeDependencies(..),
               SourceInfo(..),
               WorkspaceChanges(..),
               V0API,
               v0ApiServer)
where

import           BDCS.API.Compose(ComposeInfo(..), ComposeMsgAsk(..), ComposeMsgResp(..), ComposeStatus(..), UuidError(..), UuidStatus(..), deleteCompose, getComposesWithStatus, mkComposeStatus)
import           BDCS.API.Config(ServerConfig(..))
import           BDCS.API.Customization(processCustomization)
import           BDCS.API.Depsolve
import           BDCS.API.Error(createApiError)
import           BDCS.API.QueueStatus(QueueStatus(..), queueStatusEnded, queueStatusText)
import           BDCS.API.Recipe
import           BDCS.API.Recipes
import           BDCS.API.TOMLMediaType
import           BDCS.API.Utils(GitLock(..), applyLimits, argify, caseInsensitive, caseInsensitiveT)
import           BDCS.API.Workspace
import           BDCS.DB
import           BDCS.Builds(findBuilds, getBuild)
import           BDCS.Export.Utils(supportedOutputs)
import           BDCS.Groups(getGroupsLike)
import           BDCS.Projects(findProject, getProject, projects)
import           BDCS.Sources(findSources, getSource)
import           BDCS.Utils.Either(maybeToEither)
import           BDCS.Utils.Monad(concatMapM, mapMaybeM)
import qualified Codec.Archive.Tar as Tar
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Concurrent.STM.TChan(writeTChan)
import           Control.Concurrent.STM.TMVar(newEmptyTMVar, readTMVar)
import qualified Control.Exception as CE
import           Control.Monad.STM(atomically)
import           Control.Monad.Except
import           Data.Aeson
import           Data.Bifunctor(bimap)
import qualified Data.ByteString.Lazy as LBS
import           Data.Either(partitionEithers, rights)
import           Data.Int(Int64)
import           Data.IORef(atomicModifyIORef')
import           Data.List(find, sortBy)
import           Data.List.Extra(nubOrd)
import           Data.Maybe(fromMaybe, mapMaybe)
import           Data.String(IsString)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock(UTCTime)
import           Database.Persist.Sql
import           Data.GI.Base(GError(..))
import           Data.UUID.V4(nextRandom)
import           GHC.TypeLits(KnownSymbol)
import qualified GI.Ggit as Git
import           Servant
import           System.Directory(createDirectoryIfMissing)
import           System.FilePath.Posix((</>))


{-# ANN module ("HLint: ignore Eta reduce"  :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

defaultBranch :: Maybe String -> T.Text
defaultBranch = maybe "master" cs

-- Given a list of UUIDs, run mkComposeStatus on all of them and return only the results that
-- did not have any errors (like, from file IO).
filterMapComposeStatus :: MonadIO m => FilePath -> [T.Text] -> m [ComposeStatus]
filterMapComposeStatus dir lst = rights <$> mapM (liftIO . runExceptT . mkComposeStatus dir) lst

-- | RecipesAPIError is used to report errors with the /recipes/ routes
--
-- This is converted to a JSON error response that is used in the API responses
--
-- > {
-- >     "recipe": "unknown-recipe",
-- >     "msg": "unknown-recipe.toml is not present on branch master"
-- > }
data RecipesAPIError = RecipesAPIError
  {  raeRecipe  :: T.Text,
     raeMsg     :: T.Text
  } deriving (Eq, Show)

instance ToJSON RecipesAPIError where
  toJSON RecipesAPIError{..} = object [
      "recipe".= raeRecipe
    , "msg"   .= raeMsg ]

instance FromJSON RecipesAPIError where
  parseJSON = withObject "API Error" $ \o -> do
    raeRecipe <- o .: "recipe"
    raeMsg    <- o .: "msg"
    return RecipesAPIError{..}


-- These are the API routes. This is not documented in haddock because it doesn't format it correctly
type V0API = "projects" :> "list" :> QueryParam "offset" Int
                                  :> QueryParam "limit" Int :> Get '[JSON] ProjectsListResponse
        :<|> "projects" :> "info"     :> Capture "project_names" String :> Get '[JSON] ProjectsInfoResponse
        :<|> "projects" :> "depsolve" :> Capture "project_names" String :> Get '[JSON] ProjectsDepsolveResponse
        :<|> "errtest"  :> Get '[JSON] [T.Text]
        :<|> "blueprints"  :> "list" :> QueryParam "offset" Int
                                     :> QueryParam "limit" Int
                                     :> QueryParam "branch" String
                                     :> Get '[JSON] RecipesListResponse
        :<|> "blueprints"  :> "info" :> Capture "recipes" String
                                     :> QueryParam "branch" String
                                     :> Get '[JSON] RecipesInfoResponse
        :<|> "blueprints"  :> "changes" :> Capture "recipes" String
                                        :> QueryParam "offset" Int
                                        :> QueryParam "limit" Int
                                        :> QueryParam "branch" String
                                        :> Get '[JSON] RecipesChangesResponse
        :<|> "blueprints"  :> "new" :> ReqBody '[JSON, TOML] Recipe
                                    :> QueryParam "branch" String
                                    :> Post '[JSON] RecipesStatusResponse
        :<|> "blueprints"  :> "delete" :> Capture "recipe" String
                                       :> QueryParam "branch" String
                                       :> Delete '[JSON] RecipesStatusResponse
        :<|> "blueprints"  :> "undo" :> Capture "recipe" String
                                     :> Capture "commit" String
                                     :> QueryParam "branch" String
                                     :> Post '[JSON] RecipesStatusResponse
        :<|> "blueprints"  :> "workspace" :> ReqBody '[JSON, TOML] Recipe
                                          :> QueryParam "branch" String
                                          :> Post '[JSON] RecipesStatusResponse
        :<|> "blueprints"  :> "workspace" :> Capture "recipe" String
                                          :> QueryParam "branch" String
                                          :> Delete '[JSON] RecipesStatusResponse
        :<|> "blueprints"  :> "tag" :> Capture "recipe" String
                                    :> QueryParam "branch" String
                                    :> Post '[JSON] RecipesStatusResponse
        :<|> "blueprints"  :> "diff" :> Capture "recipe" String
                                     :> Capture "from_commit" String
                                     :> Capture "to_commit" String
                                     :> QueryParam "branch" String
                                     :> Get '[JSON] RecipesDiffResponse
        :<|> "blueprints"  :> "depsolve" :> Capture "recipes" String
                                         :> QueryParam "branch" String
                                         :> Get '[JSON] RecipesDepsolveResponse
        :<|> "blueprints"  :> "freeze" :> Capture "recipes" String
                                       :> QueryParam "branch" String
                                       :> Get '[JSON] RecipesFreezeResponse
        :<|> "modules"  :> "list" :> QueryParam "offset" Int
                                  :> QueryParam "limit" Int
                                  :> Get '[JSON] ModulesListResponse
        :<|> "modules"  :> "list" :> Capture "module_names" String
                                  :> QueryParam "offset" Int
                                  :> QueryParam "limit" Int
                                  :> Get '[JSON] ModulesListResponse
        :<|> "compose"  :> ReqBody '[JSON] ComposeBody
                        :> QueryParam "test" Int
                        :> Post '[JSON] ComposeResponse
        :<|> "compose"  :> "types" :> Get '[JSON] ComposeTypesResponse
        :<|> "compose"  :> "queue" :> Get '[JSON] ComposeQueueResponse
        :<|> "compose"  :> "finished" :> Get '[JSON] ComposeFinishedResponse
        :<|> "compose"  :> "failed" :> Get '[JSON] ComposeFailedResponse
        :<|> "compose"  :> "status" :> Capture "uuids" String
                                    :> Get '[JSON] ComposeStatusResponse
        :<|> "compose"  :> "delete" :> Capture "uuids" String
                                    :> Delete '[JSON] ComposeDeleteResponse
        :<|> "compose"  :> "logs"   :> Capture "uuid" String
                                    :> Get '[OctetStream] (Headers '[Header "Content-Disposition" String] LBS.ByteString)

-- | Connect the V0API type to all of the handlers
v0ApiServer :: ServerConfig -> Server V0API
v0ApiServer cfg = projectsListH
             :<|> projectsInfoH
             :<|> projectsDepsolveH
             :<|> errTestH
             :<|> recipesListH
             :<|> recipesInfoH
             :<|> recipesChangesH
             :<|> recipesNewH
             :<|> recipesDeleteH
             :<|> recipesUndoH
             :<|> recipesWorkspaceH
             :<|> recipesWorkspaceDeleteH
             :<|> recipesTagH
             :<|> recipesDiffH
             :<|> recipesDepsolveH
             :<|> recipesFreezeH
             :<|> modulesListH
             :<|> modulesListFilteredH
             :<|> composeH
             :<|> composeTypesH
             :<|> composeQueueH
             :<|> composeFinishedH
             :<|> composeFailedH
             :<|> composeStatusH
             :<|> composeDeleteH
             :<|> composeLogsH
  where
    projectsListH offset limit                       = projectsList cfg offset limit
    projectsInfoH project_names                      = projectsInfo cfg project_names
    projectsDepsolveH project_names                  = projectsDepsolve cfg project_names
    errTestH                                         = errTest
    recipesListH offset limit branch                 = recipesList cfg branch offset limit
    recipesInfoH recipes branch                      = recipesInfo cfg branch recipes
    recipesChangesH recipes offset limit branch      = recipesChanges cfg branch recipes offset limit
    recipesNewH recipe branch                        = recipesNew cfg branch recipe
    recipesDeleteH recipe branch                     = recipesDelete cfg branch recipe
    recipesUndoH recipe commit branch                = recipesUndo cfg branch recipe commit
    recipesWorkspaceH recipe branch                  = recipesWorkspace cfg branch recipe
    recipesWorkspaceDeleteH recipe branch            = recipesWorkspaceDelete cfg branch recipe
    recipesTagH recipe branch                        = recipesTag cfg branch recipe
    recipesDiffH recipe from_commit to_commit branch = recipesDiff cfg branch recipe from_commit to_commit
    recipesDepsolveH recipes branch                  = recipesDepsolve cfg branch recipes
    recipesFreezeH recipes branch                    = recipesFreeze cfg branch recipes
    modulesListH offset limit                        = modulesList cfg offset limit "*"
    modulesListFilteredH module_names offset limit   = modulesList cfg offset limit module_names
    composeH body test                               = compose cfg body test
    composeTypesH                                    = composeTypes
    composeQueueH                                    = composeQueue cfg
    composeFinishedH                                 = composeQueueFinished cfg
    composeFailedH                                   = composeQueueFailed cfg
    composeStatusH uuids                             = composeStatus cfg (T.splitOn "," $ cs uuids)
    composeDeleteH uuids                             = composeDelete cfg (T.splitOn "," $ cs uuids)
    composeLogsH uuid                                = composeLogs cfg uuid

-- | A test using ServantErr
errTest :: Handler [T.Text]
errTest = throwError myError
  where
    myError :: ServantErr
    myError = createApiError err503 "test_api_error" "This is a test of an API Error Response"

-- | The JSON response for /blueprints/list
data RecipesListResponse = RecipesListResponse {
    rlrRecipes  :: [T.Text],                                    -- ^ List of blueprint names
    rlrOffset   :: Int,                                         -- ^ Pagination offset into results
    rlrLimit    :: Int,                                         -- ^ Pagination limit of results
    rlrTotal    :: Int                                          -- ^ Total number of blueprint names
} deriving (Show, Eq)

instance ToJSON RecipesListResponse where
  toJSON RecipesListResponse{..} = object [
      "blueprints" .= rlrRecipes
    , "offset"  .= rlrOffset
    , "limit"   .= rlrLimit
    , "total"   .= rlrTotal ]

instance FromJSON RecipesListResponse where
  parseJSON = withObject "/blueprints/list response" $ \o -> do
    rlrRecipes <- o .: "blueprints"
    rlrOffset  <- o .: "offset"
    rlrLimit   <- o .: "limit"
    rlrTotal   <- o .: "total"
    return RecipesListResponse{..}

-- | /api/v0/blueprints/list
-- List the names of the available blueprints
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
--
-- >  {
-- >      "blueprints": [
-- >          "development",
-- >          "glusterfs",
-- >          "http-server",
-- >          "jboss",
-- >          "kubernetes",
-- >          "octave",
-- >      ],
-- >      "offset": 0,
-- >      "limit": 20,
-- >      "total": 6
-- >  }
recipesList :: ServerConfig -> Maybe String -> Maybe Int -> Maybe Int -> Handler RecipesListResponse
recipesList ServerConfig{..} mbranch moffset mlimit = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    -- TODO Figure out how to catch GitError and throw a ServantErr
    filenames <- listBranchFiles (gitRepo cfgRepoLock) (defaultBranch mbranch)
    let recipes = sortBy caseInsensitiveT $ map (T.dropEnd 5) filenames
    return $ RecipesListResponse (applyLimits limit offset recipes) offset limit (length recipes)
  where
    -- handleGitErrors :: GitError -> ServantErr
    -- handleGitErrors e = createApiError err500 "recipes_list" ("Git Error: " ++ show e)

    -- | Return the offset or the default
    offset :: Int
    offset = fromMaybe 0 moffset

    -- | Return the limit or the default
    limit :: Int
    limit  = fromMaybe 20 mlimit


-- | Status of a blueprint's workspace
data WorkspaceChanges = WorkspaceChanges {
    wcName      :: T.Text,                                              -- ^ Blueprint name
    wcChanged   :: Bool                                                 -- ^ True when it is newer than the last commit
} deriving (Show, Eq)
instance ToJSON WorkspaceChanges where
  toJSON WorkspaceChanges{..} = object [
      "name"    .= wcName
    , "changed" .= wcChanged ]

instance FromJSON WorkspaceChanges where
  parseJSON = withObject "workspace changes" $ \o -> do
    wcName    <- o .: "name"
    wcChanged <- o .: "changed"
    return WorkspaceChanges{..}


-- | The JSON response for /blueprints/info
data RecipesInfoResponse = RecipesInfoResponse {
    rirChanges  :: [WorkspaceChanges],                                  -- ^ Workspace status for each blueprint
    rirRecipes  :: [Recipe],                                            -- ^ The Recipe record
    rirErrors   :: [RecipesAPIError]                                    -- ^ Errors reading the blueprint
} deriving (Show, Eq)

instance ToJSON RecipesInfoResponse where
  toJSON RecipesInfoResponse{..} = object [
      "changes"   .= rirChanges
    , "blueprints" .= rirRecipes
    , "errors"  .= rirErrors ]

instance FromJSON RecipesInfoResponse where
  parseJSON = withObject "/blueprints/info response" $ \o -> do
    rirChanges <- o .: "changes"
    rirRecipes <- o .: "blueprints"
    rirErrors  <- o .: "errors"
    return RecipesInfoResponse{..}


-- | /api/v0/blueprints/info/\<recipes\>
-- Return the contents of the blueprint, or a list of recipes
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipes_names@]: A comma separated list of blueprint names
--
-- The errors list may be empty, or may include blueprint-specific errors if
-- there was a problem retrieving it.
--
-- > {
-- >     "changes": [
-- >         {
-- >             "name": "blueprint-test",
-- >             "changed": true
-- >         },
-- >     ],
-- >     "blueprints": [
-- >         {
-- >             "name": "http-server",
-- >             "description": "An example http server with PHP and MySQL support.",
-- >             "version": "0.0.1",
-- >             "modules": [
-- >                 {
-- >                     "name": "httpd",
-- >                     "version": "2.4.*"
-- >                 },
-- >                 {
-- >                     "name": "mod_auth_kerb",
-- >                     "version": "5.4"
-- >                 },
-- >                 {
-- >                     "name": "mod_ssl",
-- >                     "version": "2.4.*"
-- >                 },
-- >                 {
-- >                     "name": "php",
-- >                     "version": "5.4.*"
-- >                 },
-- >                 {
-- >                     "name": "php-mysql",
-- >                     "version": "5.4.*"
-- >                 }
-- >             ],
-- >             "packages": [
-- >                 {
-- >                     "name": "tmux",
-- >                     "version": "2.2"
-- >                 },
-- >                 {
-- >                     "name": "openssh-server",
-- >                     "version": "6.6.*"
-- >                 },
-- >                 {
-- >                     "name": "rsync",
-- >                     "version": "3.0.*"
-- >                 }
-- >             ]
-- >         },
-- >     "errors": [
-- >         {
-- >             "blueprint": "a-missing-blueprint",
-- >             "msg": "Error retrieving a-missing-blueprint"
-- >         }
-- >     ]
-- > }
--
recipesInfo :: ServerConfig -> Maybe String -> String -> Handler RecipesInfoResponse
recipesInfo ServerConfig{..} branch recipe_names = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    let recipe_name_list = map T.pack (argify [recipe_names])
    (changes, recipes, errors) <- allRecipeInfo recipe_name_list [] [] []
    return $ RecipesInfoResponse changes recipes errors
  where
    allRecipeInfo :: [T.Text] -> [WorkspaceChanges] -> [Recipe] -> [RecipesAPIError] -> IO ([WorkspaceChanges], [Recipe], [RecipesAPIError])
    allRecipeInfo [] _ _ _ = return ([], [], [])
    allRecipeInfo [recipe_name] changes_list recipes_list errors_list =
                  oneRecipeInfo recipe_name changes_list recipes_list errors_list
    allRecipeInfo (recipe_name:xs) changes_list recipes_list errors_list = do
                  (new_changes, new_recipes, new_errors) <- oneRecipeInfo recipe_name changes_list recipes_list errors_list
                  allRecipeInfo xs new_changes new_recipes new_errors

    oneRecipeInfo :: T.Text -> [WorkspaceChanges] -> [Recipe] -> [RecipesAPIError] -> IO ([WorkspaceChanges], [Recipe], [RecipesAPIError])
    oneRecipeInfo recipe_name changes_list recipes_list errors_list = do
        result <- getRecipeInfo cfgRepoLock (defaultBranch branch) recipe_name
        return (new_changes result, new_recipes result, new_errors result)
      where
        new_errors :: Either String (Bool, Recipe) -> [RecipesAPIError]
        new_errors (Left err)  = RecipesAPIError recipe_name (T.pack err):errors_list
        new_errors (Right _) = errors_list

        new_changes :: Either String (Bool, Recipe) -> [WorkspaceChanges]
        new_changes (Right (changed, _)) = WorkspaceChanges recipe_name changed:changes_list
        new_changes (Left _)             = changes_list

        new_recipes :: Either String (Bool, Recipe) -> [Recipe]
        new_recipes (Right (_, recipe)) = recipe:recipes_list
        new_recipes (Left _)            = recipes_list

-- | Get the recipe from the workspace or from git
-- If there is neither workspace or git recipes then an error is returned.
getRecipeInfo :: GitLock -> T.Text -> T.Text -> IO (Either String (Bool, Recipe))
getRecipeInfo repoLock branch recipe_name = do
    --   read the workspace recipe if it exists, errors are mapped to Nothing
    ws_recipe <- catch_ws_recipe
    --   read the git recipe (if it exists), Errors are mapped to Left
    git_recipe <- catch_git_recipe

    case (ws_recipe, git_recipe) of
        (Nothing,     Left e)       -> return $ Left e
        (Just recipe, Left _)       -> return $ Right (True, recipe)
        (Nothing,     Right recipe) -> return $ Right (False, recipe)
        (Just ws_r,   Right git_r)  -> return $ Right (ws_r /= git_r, ws_r)
  where
    -- | Read the recipe from the workspace, and convert WorkspaceErrors into Nothing
    catch_ws_recipe :: IO (Maybe Recipe)
    catch_ws_recipe =
        CE.catch (workspaceRead (gitRepo repoLock) branch recipe_name)
                 (\(_ :: WorkspaceError) -> return Nothing)

    -- | Read the recipe from git, and convert errors into Left descriptions of what went wrong.
    catch_git_recipe :: IO (Either String Recipe)
    catch_git_recipe =
        CE.catches (readRecipeCommit (gitRepo repoLock) branch recipe_name Nothing)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | Details about commits to a blueprint
data RecipeChanges = RecipeChanges {
    rcName      :: T.Text,                                              -- ^ Blueprint name
    rcChange    :: [CommitDetails],                                     -- ^ Details of the commit
    rcTotal     :: Int                                                  -- ^ Total number of commits
} deriving (Show, Eq)

instance ToJSON RecipeChanges where
  toJSON RecipeChanges{..} = object [
      "name"   .= rcName
    , "change" .= rcChange
    , "total"  .= rcTotal ]

instance FromJSON RecipeChanges where
  parseJSON = withObject "blueprint changes" $ \o -> do
    rcName   <- o .: "name"
    rcChange <- o .: "change"
    rcTotal  <- o .: "total"
    return RecipeChanges{..}


-- The JSON response for /blueprints/changes
data RecipesChangesResponse = RecipesChangesResponse {
    rcrRecipes  :: [RecipeChanges],                                     -- ^ Changes for each blueprint
    rcrErrors   :: [RecipesAPIError],                                   -- ^ Any errors for the requested changes
    rcrOffset   :: Int,                                                 -- ^ Pagination offset
    rcrLimit    :: Int                                                  -- ^ Pagination limit
} deriving (Show, Eq)

instance ToJSON RecipesChangesResponse where
  toJSON RecipesChangesResponse{..} = object [
      "blueprints" .= rcrRecipes
    , "errors" .= rcrErrors
    , "offset" .= rcrOffset
    , "limit"  .= rcrLimit ]

instance FromJSON RecipesChangesResponse where
  parseJSON = withObject "/blueprints/changes/ response" $ \o -> do
    rcrRecipes <- o .: "blueprints"
    rcrErrors  <- o .: "errors"
    rcrOffset  <- o .: "offset"
    rcrLimit   <- o .: "limit"
    return RecipesChangesResponse{..}


-- | /api/v0/blueprints/changes/\<recipes\>
-- Return the commit history of the blueprints
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipes_name@]: The blueprint name
-- [@moffset@]: The offset from the start of the results. Defaults to 0
-- [@mlimit@]: Limit to the number of results to be returned. Defaults to 20
--
-- The changes for each listed blueprint will have offset and limit applied to them.
-- This means that there will be cases where changes will be empty, when offset > total
-- for the blueprint.
--
-- If a blueprint commit has been tagged as a new revision the changes will include a
-- `revision` field set to the revision number. If the commit has not been tagged it
-- will not have this field included.
--
-- > {
-- >     "blueprints": [
-- >         {
-- >             "name": "nfs-server",
-- >             "changes": [
-- >                 {
-- >                     "commit": "97d483e8dd0b178efca9a805e5fd8e722c48ac8e",
-- >                     "time": "Wed,  1 Mar 2017 13:29:37 -0800",
-- >                     "summary": "Recipe nfs-server saved"
-- >                 },
-- >                 {
-- >                     "commit": "857e1740f983bf033345c3242204af0ed7b81f37",
-- >                     "time": "Wed,  1 Mar 2017 09:28:53 -0800",
-- >                     "summary": "Recipe nfs-server saved",
-- >                     "revision" : 1
-- >                 }
-- >             ],
-- >             "total": 2
-- >         },
-- >         {
-- >             "name": "ruby",
-- >             "changes": [
-- >                 {
-- >                     "commit": "4b84f072befc3f4debbe1348d6f4b166f7c83d78",
-- >                     "time": "Wed,  1 Mar 2017 13:32:09 -0800",
-- >                     "summary": "Recipe ruby saved"
-- >                 },
-- >                 {
-- >                     "commit": "85999253c1790367a860a344ea622971b7e0a050",
-- >                     "time": "Wed,  1 Mar 2017 13:31:19 -0800",
-- >                     "summary": "Recipe ruby saved"
-- >                 }
-- >             ],
-- >             "total": 2
-- >         }
-- >     ],
-- >     "errors": [
-- >         {
-- >             "blueprint": "a-missing-recipe",
-- >             "msg": "Error retrieving a-missing-blueprint"
-- >         }
-- >     ]
-- >     "offset": 0,
-- >     "limit": 20
-- > }
recipesChanges :: ServerConfig -> Maybe String -> String -> Maybe Int -> Maybe Int -> Handler RecipesChangesResponse
recipesChanges ServerConfig{..} mbranch recipe_names moffset mlimit = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    let recipe_name_list = map T.pack (argify [recipe_names])
    (changes, errors) <- allRecipeChanges recipe_name_list [] []
    return $ RecipesChangesResponse changes errors offset limit
  where
    allRecipeChanges :: [T.Text] -> [RecipeChanges] -> [RecipesAPIError] -> IO ([RecipeChanges], [RecipesAPIError])
    allRecipeChanges [] _ _ = return ([], [])
    allRecipeChanges [recipe_name] changes_list errors_list =
                     oneRecipeChange recipe_name changes_list errors_list
    allRecipeChanges (recipe_name:xs) changes_list errors_list = do
                     (new_changes, new_errors) <- oneRecipeChange recipe_name changes_list errors_list
                     allRecipeChanges xs new_changes new_errors

    oneRecipeChange :: T.Text -> [RecipeChanges] -> [RecipesAPIError] -> IO ([RecipeChanges], [RecipesAPIError])
    oneRecipeChange recipe_name changes_list errors_list = do
        result <- catch_recipe_changes recipe_name
        return (new_changes result, new_errors result)
      where
        new_changes :: Either String [CommitDetails] -> [RecipeChanges]
        new_changes (Right changes) = RecipeChanges recipe_name (applyLimits limit offset changes) (length $ applyLimits limit offset changes):changes_list
        new_changes (Left _)        = changes_list

        new_errors :: Either String [CommitDetails] -> [RecipesAPIError]
        new_errors (Left err) = RecipesAPIError recipe_name (T.pack err):errors_list
        new_errors (Right _)  = errors_list

    offset :: Int
    offset = fromMaybe 0 moffset

    limit :: Int
    limit  = fromMaybe 20 mlimit

    catch_recipe_changes :: T.Text -> IO (Either String [CommitDetails])
    catch_recipe_changes recipe_name =
        CE.catches (Right <$> listRecipeCommits (gitRepo cfgRepoLock) (defaultBranch mbranch) recipe_name)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | JSON status response
data RecipesStatusResponse = RecipesStatusResponse {
    rsrStatus :: Bool,                                                  -- ^ Success/Failure of the request
    rsrErrors :: [RecipesAPIError]                                      -- ^ Errors
} deriving (Show, Eq)

instance ToJSON RecipesStatusResponse where
  toJSON RecipesStatusResponse{..} = object [
      "status" .= rsrStatus
    , "errors" .= rsrErrors ]

instance FromJSON RecipesStatusResponse where
  parseJSON = withObject "/blueprints/* status response" $ \o -> do
    rsrStatus <- o .: "status"
    rsrErrors <- o .: "errors"
    return RecipesStatusResponse{..}


-- | POST /api/v0/blueprints/new
-- Create or update a blueprint.
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe@]: The blueprint record
--
-- The body of the post is a JSON or TOML representation of the blueprint. If Conten-Type is application/json
-- it uses the same format received from /api/v0/blueprints/info/\<blueprints\>, and if it is text/x-toml it uses
-- the blueprint's TOML format for the body.
--
-- The response for a successful POST is:
--
-- > {
-- >     "status": true,
-- >     "errors": []
-- > }
recipesNew :: ServerConfig -> Maybe String -> Recipe -> Handler RecipesStatusResponse
recipesNew ServerConfig{..} mbranch recipe = liftIO $ RWL.withWrite (gitRepoLock cfgRepoLock) $ do
    result <- catch_recipe_new
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError "Unknown" (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_new :: IO (Either String Git.OId)
    catch_recipe_new =
        CE.catches (Right <$> commitRecipe (gitRepo cfgRepoLock) (defaultBranch mbranch) recipe)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | DELETE /api/v0/blueprints/delete/\<recipe\>
-- Delete the named blueprint from the repository branch
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe_name@]: The blueprint name
--
-- The response for a successful DELETE is:
--
-- > {
-- >     "status": true,
-- >     "errors": []
-- > }
recipesDelete :: ServerConfig -> Maybe String -> String -> Handler RecipesStatusResponse
recipesDelete ServerConfig{..} mbranch recipe_name = liftIO $ RWL.withWrite (gitRepoLock cfgRepoLock) $ do
    result <- catch_recipe_delete
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError (T.pack recipe_name) (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_delete :: IO (Either String Git.OId)
    catch_recipe_delete =
        CE.catches (Right <$> deleteRecipe (gitRepo cfgRepoLock) (defaultBranch mbranch) (T.pack recipe_name))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | POST /api/v0/blueprints/undo/\<recipe\>/\<commit\>
-- Revert a blueprint to a previous commit
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe_name@]: The blueprint name
-- [@commit@]: The commit to revert to
--
-- The response for a successful POST is:
--
-- > {
-- >     "status": true,
-- >     "errors": []
-- > }
recipesUndo :: ServerConfig -> Maybe String -> String -> String -> Handler RecipesStatusResponse
recipesUndo ServerConfig{..} mbranch recipe_name commit = liftIO $ RWL.withWrite (gitRepoLock cfgRepoLock) $ do
    result <- catch_recipe_undo
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError (T.pack recipe_name) (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_undo :: IO (Either String Git.OId)
    catch_recipe_undo =
        CE.catches (Right <$> revertRecipe (gitRepo cfgRepoLock) (defaultBranch mbranch) (T.pack recipe_name) (T.pack commit))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | POST /api/v0/blueprints/workspace
-- Update the temporary blueprint workspace
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe@]: The blueprint record
--
-- The body of the post is the same as /blueprints/new/. For more details on the
-- workspace see "BDCS.API.Workspace"
--
-- The response for a successful POST is:
--
-- > {
-- >     "status": true,
-- >     "errors": []
-- > }
recipesWorkspace :: ServerConfig -> Maybe String -> Recipe -> Handler RecipesStatusResponse
recipesWorkspace ServerConfig{..} mbranch recipe = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    result <- catch_recipe_ws
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError "Unknown" (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_ws :: IO (Either String ())
    catch_recipe_ws =
        CE.catches (Right <$> workspaceWrite (gitRepo cfgRepoLock) (defaultBranch mbranch) recipe)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | DELETE /api/v0/blueprints/workspace/\<recipe\>
-- Delete the named blueprint from the workspace
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe_name@]: The blueprint name
--
-- The response for a successful DELETE is:
--
-- > {
-- >     "status": true,
-- >     "errors": []
-- > }
recipesWorkspaceDelete :: ServerConfig -> Maybe String -> String -> Handler RecipesStatusResponse
recipesWorkspaceDelete ServerConfig{..} mbranch recipe_name = liftIO $ RWL.withWrite (gitRepoLock cfgRepoLock) $ do
    result <- catch_recipe_delete
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError (T.pack recipe_name) (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_delete :: IO (Either String ())
    catch_recipe_delete =
        CE.catches (Right <$> workspaceDelete (gitRepo cfgRepoLock) (defaultBranch mbranch) (T.pack recipe_name))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | POST /api/v0/blueprints/tag/<blueprint>
-- Tag the most recent blueprint commit as the next revision
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe_name@]: The blueprint name
--
-- If the commit is already tagged it will return False.
--
-- The response for a successful POST is:
--
-- > {
-- >     "status": true,
-- >     "errors": []
-- > }
recipesTag :: ServerConfig -> Maybe String -> String -> Handler RecipesStatusResponse
recipesTag ServerConfig{..} mbranch recipe_name = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    result <- catch_recipe_tag
    case result of
        Left  err    -> return $ RecipesStatusResponse False [RecipesAPIError "Unknown" (T.pack err)]
        Right status -> return $ RecipesStatusResponse status []
  where
    catch_recipe_tag :: IO (Either String Bool)
    catch_recipe_tag =
        CE.catches (Right <$> tagRecipeCommit (gitRepo cfgRepoLock) (defaultBranch mbranch) (T.pack recipe_name))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | JSON response for /blueprints/diff
data RecipesDiffResponse = RecipesDiffResponse {
    rdrDiff :: [RecipeDiffEntry]
} deriving (Eq, Show)

instance ToJSON RecipesDiffResponse where
  toJSON RecipesDiffResponse{..} = object [
      "diff" .= rdrDiff ]

instance FromJSON RecipesDiffResponse where
  parseJSON = withObject "/blueprints/diff response" $ \o -> do
    rdrDiff <- o .: "diff"
    return RecipesDiffResponse{..}

-- | /api/v0/blueprints/diff/<blueprint>/<from_commit>/<to_commit>
-- Return the diff between the two blueprint commits.
--
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe_name@]: The blueprint name
-- [@from_commit@]: The older commit to caclulate the difference from, can also be NEWEST
-- [@to_commit@]: The newer commit to calculate the diff. to, can also be NEWEST or WORKSPACE
--
-- If there is an error retrieving a commit (eg. it cannot find the hash), it will use HEAD
-- instead and log an error.
--
--
-- In addition to the commit hashes listed by a call to /blueprints/changes/\<blueprint-name\> you
-- can use NEWEST to compare the latest commit, and WORKSPACE to compare it with
-- the current temporary workspace version of the blueprint. eg. to see what the differences
-- are between the current workspace and most recent commit of http-server you would call:
--
-- > /blueprints/diff/http-server/NEWEST/WORKSPACE
--
-- Each entry in the response's diff object contains the old blueprint value and the new one.
-- If old is null and new is set, then it was added.
-- If new is null and old is set, then it was removed.
-- If both are set, then it was changed.
--
-- The old/new entries will have the name of the blueprint field that was changed. This
-- can be one of: Name, Description, Version, Module, or Package.
-- The contents for these will be the old/new values for them.
--
-- In the example below the description and version were changed. The php module's
-- version was changed, the rsync package was removed, and the vim-enhanced package
-- was added.
--
-- # Examples
--
-- > {
-- >     "diff": [
-- >         {
-- >             "old": {
-- >                 "Description": "An example http server with PHP and MySQL support."
-- >             },
-- >             "new": {
-- >                 "Description": "Apache HTTP Server"
-- >             }
-- >         },
-- >         {
-- >             "old": {
-- >                 "Version": "0.0.1"
-- >             },
-- >             "new": {
-- >                 "Version": "0.1.1"
-- >             }
-- >         },
-- >         {
-- >             "old": {
-- >                 "Module": {
-- >                     "name": "php",
-- >                     "version": "5.4.*"
-- >                 }
-- >             },
-- >             "new": {
-- >                 "Module": {
-- >                     "name": "php",
-- >                     "version": "5.6.*"
-- >                 }
-- >             }
-- >         },
-- >         {
-- >             "old": null,
-- >             "new": {
-- >                 "Package": {
-- >                     "name": "vim-enhanced",
-- >                     "version": "8.0.*"
-- >                 }
-- >             }
-- >         },
-- >         {
-- >             "old": {
-- >                 "Package": {
-- >                     "name": "rsync",
-- >                     "version": "3.0.*"
-- >                 }
-- >             },
-- >             "new": null
-- >         }
-- >     ]
-- > }
recipesDiff :: ServerConfig -> Maybe String -> String -> String -> String -> Handler RecipesDiffResponse
recipesDiff ServerConfig{..} mbranch recipe_name from_commit to_commit = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    -- Setup old_recipe
    -- NEWEST == read the latest git commit for recipe_name
    -- Otherwise try to read the passed-in commit hash string
    old_recipe <- get_recipe from_commit

    -- Setup new_recipe
    -- WORKSPACE == read the recipe's workspace
    -- NEWEST == read the latest git commit for recipe_name
    -- Otherwise try to read the passed-in commit hash string
    new_recipe <- get_recipe to_commit

    case (old_recipe, new_recipe) of
        (Left _, _)     -> return $ RecipesDiffResponse []
        (_, Left _)     -> return $ RecipesDiffResponse []
        (Right o, Right n) -> do
            let diff = recipeDiff o n
            return $ RecipesDiffResponse diff
  where
    get_recipe :: String -> IO (Either String Recipe)
    get_recipe "NEWEST"    = catch_git_recipe (T.pack recipe_name) Nothing
    get_recipe "WORKSPACE" = do
        ws_recipe <- catch_ws_recipe (T.pack recipe_name)
        -- If there is no workspace recipe fall back to most recent commit
        case ws_recipe of
            Just recipe -> return $ Right recipe
            Nothing     -> get_recipe "NEWEST"
    get_recipe commit      = catch_git_recipe (T.pack recipe_name) (Just $ T.pack commit)

    -- | Read the recipe from the workspace, and convert WorkspaceErrors into Nothing
    catch_ws_recipe :: T.Text -> IO (Maybe Recipe)
    catch_ws_recipe name =
        CE.catch (workspaceRead (gitRepo cfgRepoLock) (defaultBranch mbranch) name)
                 (\(_ :: WorkspaceError) -> return Nothing)

    -- | Read the recipe from git, and convert errors into Left descriptions of what went wrong.
    catch_git_recipe :: T.Text -> Maybe T.Text -> IO (Either String Recipe)
    catch_git_recipe name commit =
        CE.catches (readRecipeCommit (gitRepo cfgRepoLock) (defaultBranch mbranch) name commit)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | The blueprint's dependency details
data RecipeDependencies = RecipeDependencies {
    rdRecipe       :: Recipe,
    rdDependencies :: [PackageNEVRA],
    rdModules      :: [PackageNEVRA]
} deriving (Show, Eq)

instance ToJSON RecipeDependencies where
  toJSON RecipeDependencies{..} = object [
      "blueprint"       .= rdRecipe
    , "dependencies" .= rdDependencies
    , "modules"      .= rdModules ]

instance FromJSON RecipeDependencies where
  parseJSON = withObject "blueprint dependencies" $ \o -> do
    rdRecipe       <- o .: "blueprint"
    rdDependencies <- o .: "dependencies"
    rdModules      <- o .: "modules"
    return RecipeDependencies{..}


-- | The JSON response for /blueprints/depsolve/<blueprints>
data RecipesDepsolveResponse = RecipesDepsolveResponse {
    rdrRecipes  :: [RecipeDependencies],                             -- ^ List of blueprints and their dependencies
    rdrErrors   :: [RecipesAPIError]                                 -- ^ Errors reading the blueprint
} deriving (Show, Eq)

instance ToJSON RecipesDepsolveResponse where
  toJSON RecipesDepsolveResponse{..} = object [
      "blueprints" .= rdrRecipes
    , "errors"  .= rdrErrors ]

instance FromJSON RecipesDepsolveResponse where
  parseJSON = withObject "/blueprints/depsolve response" $ \o -> do
    rdrRecipes <- o .: "blueprints"
    rdrErrors  <- o .: "errors"
    return RecipesDepsolveResponse{..}

-- | /api/v0/blueprints/depsolve/<blueprints>
-- Return the blueprint and summary information about all of its modules and packages.
--
-- [@pool@]: The sqlite connection pool object
-- [@repoLock@]: The git repositories `ReadWriteLock` and Repository object
-- [@mbranch@]: The branch name
-- [@recipe_names@]: The blueprint names to depsolve, comma-separated if there is more than one
--
-- If a workspace version of the blueprint is found it will be used for the depsolve. If there are
-- any errors reading the blueprint, or depsolving it, they will be returned in the 'errors' object.
--
-- # Error example
--
-- > {
-- >     "errors": [
-- >         {
-- >             "msg": "nfs-server.toml is not present on branch master",
-- >             "blueprint": "nfs-server"
-- >         }
-- >     ],
-- >     "blueprints": []
-- > }
--
--
-- A successful result will include 3 items. 'dependencies' will be the NEVRAs of all of the
-- projects needed to satisfy the blueprint's dependencies. 'modules' will be the project NEVRAs
-- for the modules and packages explicitly listed in the blueprint, and 'blueprint' will be a copy of
-- the blueprint that was depsolved.
--
-- # Abbreviated successful example
--
-- > {
-- >     "errors": [],
-- >     "blueprints": [
-- >         {
-- >             "dependencies": [
-- >                 {
-- >                     "arch": "x86_64",
-- >                     "epoch": 0,
-- >                     "name": "apr",
-- >                     "release": "3.el7",
-- >                     "version": "1.4.8"
-- >                 },
-- >                 {
-- >                     "arch": "x86_64",
-- >                     "epoch": 0,
-- >                     "name": "apr-util",
-- >                     "release": "6.el7",
-- >                     "version": "1.5.2"
-- >                 },
-- >                 ...
-- >             ],
-- >             "modules": [
-- >                 {
-- >                     "arch": "x86_64",
-- >                     "epoch": 0,
-- >                     "name": "httpd",
-- >                     "release": "67.el7",
-- >                     "version": "2.4.6"
-- >                 },
-- >                 {
-- >                     "arch": "x86_64",
-- >                     "epoch": 0,
-- >                     "name": "mod_auth_kerb",
-- >                     "release": "28.el7",
-- >                     "version": "5.4"
-- >                 },
-- >                 ...
-- >             ],
-- >            "blueprint": {
-- >                 "description": "An example http server with PHP and MySQL support.",
-- >                 "modules": [
-- >                     {
-- >                         "name": "httpd",
-- >                         "version": "2.4.*"
-- >                     },
-- >                     {
-- >                         "name": "mod_auth_kerb",
-- >                         "version": "5.4"
-- >                     },
-- >                     {
-- >                         "name": "mod_ssl",
-- >                         "version": "2.4.*"
-- >                     },
-- >                     {
-- >                         "name": "php",
-- >                         "version": "5.4.*"
-- >                     },
-- >                     {
-- >                         "name": "php-mysql",
-- >                         "version": "5.4.*"
-- >                     }
-- >                 ],
-- >                 "name": "http-server",
-- >                 "packages": [
-- >                     {
-- >                         "name": "tmux",
-- >                         "version": "2.2"
-- >                     },
-- >                     {
-- >                         "name": "openssh-server",
-- >                         "version": "6.6.*"
-- >                     },
-- >                     {
-- >                         "name": "rsync",
-- >                         "version": "3.0.*"
-- >                     }
-- >                 ],
-- >                 "version": "0.2.0"
-- >             }
-- >         }
-- >     ]
-- > }
recipesDepsolve :: ServerConfig -> Maybe String -> String -> Handler RecipesDepsolveResponse
recipesDepsolve ServerConfig{..} mbranch recipe_names = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    let recipe_name_list = map T.pack (argify [recipe_names])
    (recipes, errors) <- liftIO $ allRecipeDeps recipe_name_list
    return $ RecipesDepsolveResponse recipes errors
  where
    allRecipeDeps :: [T.Text] -> IO ([RecipeDependencies], [RecipesAPIError])
    allRecipeDeps recipeNames = do
        -- Convert the list of names into a list of Recipes.  Also collect a list of errors
        -- that occurred while doing the conversion.  We don't simply stop on the first error.
        results <- mapM (getOneRecipeInfo cfgRepoLock (defaultBranch mbranch)) recipeNames
        let (errors, recipes) = partitionEithers results

        -- Depsolve each recipe, also gathering up any errors from this process as well.  Because
        -- depsolveRecipe lives elsewhere and therefore cannot return types defined in this file,
        -- it returns more generic things lacking the recipe name.  Thus, here we must convert
        -- both possibilities of the return type.
        results' <- mapM (\r -> bimap (toRecipesAPIError r)
                                      (toRecipeDependencies r)
                                <$> depsolveRecipe cfgPool r)
                         recipes
        let (depErrors, deps) = partitionEithers results'
        return (deps, errors ++ depErrors)

    toRecipesAPIError :: Recipe -> T.Text -> RecipesAPIError
    toRecipesAPIError Recipe{..} msg =
        RecipesAPIError { raeRecipe=cs rName, raeMsg=msg }

    toRecipeDependencies :: Recipe -> ([PackageNEVRA], [PackageNEVRA]) -> RecipeDependencies
    toRecipeDependencies recipe (deps, mods) =
        RecipeDependencies { rdRecipe=recipe, rdDependencies=deps, rdModules=mods }

    getOneRecipeInfo :: GitLock -> T.Text -> T.Text -> IO (Either RecipesAPIError Recipe)
    getOneRecipeInfo lock branch name =
        getRecipeInfo lock branch name >>= \case
            Left err     -> return $ Left RecipesAPIError { raeRecipe=name, raeMsg=cs err }
            Right (_, r) -> return $ Right r


-- | The JSON response for /blueprints/freeze/<blueprints>
data RecipesFreezeResponse = RecipesFreezeResponse {
    rfrRecipes  :: [Recipe],                                         -- ^ Recipes with exact versions
    rfrErrors   :: [RecipesAPIError]                                 -- ^ Errors reading the blueprint
} deriving (Show, Eq)

instance ToJSON RecipesFreezeResponse where
  toJSON RecipesFreezeResponse{..} = object [
      "blueprints" .= rfrRecipes
    , "errors"  .= rfrErrors ]

instance FromJSON RecipesFreezeResponse where
  parseJSON = withObject "/blueprints/freeze response" $ \o -> do
    rfrRecipes <- o .: "blueprints"
    rfrErrors  <- o .: "errors"
    return RecipesFreezeResponse{..}

-- | /api/v0/blueprints/freeze/<blueprints>
-- Return the contents of the blueprint with frozen dependencies instead of expressions.
--
-- This depsolves the blueprint, and then replaces the modules and packages versions with
-- the EVR found by the depsolve, returning a frozen blueprint.
--
-- # Examples
--
-- > {
-- >     "errors": [],
-- >     "blueprints": [
-- >         {
-- >             "description": "An example http server with PHP and MySQL support.",
-- >             "modules": [
-- >                 {
-- >                     "name": "httpd",
-- >                     "version": "2.4.6-67.el7"
-- >                 },
-- >                 {
-- >                     "name": "mod_auth_kerb",
-- >                     "version": "5.4-28.el7"
-- >                 },
-- >                 {
-- >                     "name": "mod_ssl",
-- >                     "version": "1:2.4.6-67.el7"
-- >                 },
-- >                 {
-- >                     "name": "php",
-- >                     "version": "5.4.16-42.el7"
-- >                 },
-- >                 {
-- >                     "name": "php-mysql",
-- >                     "version": "5.4.16-42.el7"
-- >                 }
-- >             ],
-- >             "name": "http-server",
-- >             "packages": [
-- >                 {
-- >                     "name": "tmux",
-- >                     "version": "1.8-4.el7"
-- >                 },
-- >                 {
-- >                     "name": "openssh-server",
-- >                     "version": "7.4p1-11.el7"
-- >                 },
-- >                 {
-- >                     "name": "rsync",
-- >                     "version": "3.0.9-18.el7"
-- >                 }
-- >             ],
-- >             "version": "0.2.0"
-- >         }
-- >     ]
-- > }
recipesFreeze :: ServerConfig -> Maybe String -> String -> Handler RecipesFreezeResponse
recipesFreeze ServerConfig{..} mbranch recipe_names = liftIO $ RWL.withRead (gitRepoLock cfgRepoLock) $ do
    let recipe_name_list = map T.pack (argify [recipe_names])
    (recipes, errors) <- liftIO $ allRecipeDeps recipe_name_list
    return $ RecipesFreezeResponse recipes errors
  where
    allRecipeDeps :: [T.Text] -> IO ([Recipe], [RecipesAPIError])
    allRecipeDeps recipeNames = do
        -- Convert the list of names into a list of Recipes.  Also collect a list of errors
        -- that occurred while doing the conversion.  We don't simply stop on the first error.
        results <- mapM (getOneRecipeInfo cfgRepoLock (defaultBranch mbranch)) recipeNames
        let (errors, recipes) = partitionEithers results

        -- Depsolve each recipe, also gathering up any errors from this process as well.  Because
        -- depsolveRecipe lives elsewhere and therefore cannot return types defined in this file,
        -- it returns more generic things lacking the recipe name.  Thus, here we must convert
        -- both possibilities of the return type.
        --
        -- Additionally, here we must replace everything with the frozen version numbers.
        results' <- mapM (\r -> bimap (toRecipesAPIError r)
                                      (frozenRecipe r)
                                <$> depsolveRecipe cfgPool r)
                         recipes
        let (depErrors, recipes') = partitionEithers results'
        return (recipes', errors ++ depErrors)

    toRecipesAPIError :: Recipe -> T.Text -> RecipesAPIError
    toRecipesAPIError Recipe{..} msg =
        RecipesAPIError { raeRecipe=cs rName, raeMsg=msg }

    getOneRecipeInfo :: GitLock -> T.Text -> T.Text -> IO (Either RecipesAPIError Recipe)
    getOneRecipeInfo lock branch name =
        getRecipeInfo lock branch name >>= \case
            Left err     -> return $ Left RecipesAPIError { raeRecipe=name, raeMsg=cs err }
            Right (_, r) -> return $ Right r

    -- Replace the recipe's module and package versions with the EVR selected by depsolving
    frozenRecipe :: Recipe -> ([PackageNEVRA], [PackageNEVRA]) -> Recipe
    frozenRecipe recipe (dep_nevras, _) = do
        let new_modules = getFrozenModules (rModules recipe) dep_nevras
        let new_packages= getFrozenModules (rPackages recipe) dep_nevras
        recipe { rModules = new_modules, rPackages = new_packages }

    -- Get a frozen list of projects using the depsolved NEVRAs
    getFrozenModules :: [RecipeModule] -> [PackageNEVRA] -> [RecipeModule]
    getFrozenModules recipe_modules all_nevras = mapMaybe (getFrozenRecipeModule all_nevras) recipe_modules

    getFrozenRecipeModule :: [PackageNEVRA] -> RecipeModule -> Maybe RecipeModule
    getFrozenRecipeModule all_nevras recipe_module =
        lookupRecipeModule recipe_module all_nevras >>= \module_nevra ->
                                                        Just (frozenRecipeModule recipe_module module_nevra)

    -- Lookup a RecipeModule in the list of depsolved packages
    lookupRecipeModule :: RecipeModule -> [PackageNEVRA] -> Maybe PackageNEVRA
    lookupRecipeModule recipe_module all_nevras = find (\e -> pnName e == T.pack (rmName recipe_module)) all_nevras

    -- Create a new RecipeModule with frozen version
    frozenRecipeModule :: RecipeModule -> PackageNEVRA -> RecipeModule
    frozenRecipeModule rm pn = rm { rmVersion = getVersionFromNEVRA pn }

    -- Convert a PackageNEVRA to a string for RecipeModule
    -- eg. 2:3.1.4-22.fc27
    getVersionFromNEVRA :: PackageNEVRA -> String
    getVersionFromNEVRA nevra = T.unpack $ T.concat [epoch $ pnEpoch nevra, pnVersion nevra, "-", pnRelease nevra]
      where
        epoch Nothing  = ""
        epoch (Just e) = T.pack (show e) `T.append` ":"

-- | The JSON response for /projects/list
data ProjectsListResponse = ProjectsListResponse {
    plpProjects :: [Projects],                                  -- ^ List of project names
    plpOffset   :: Int,                                         -- ^ Pagination offset into results
    plpLimit    :: Int,                                         -- ^ Pagination limit of results
    plpTotal    :: Int                                          -- ^ Total number of project names
} deriving (Show, Eq)

instance ToJSON ProjectsListResponse where
  toJSON ProjectsListResponse{..} = object [
      "projects" .= plpProjects
    , "offset"   .= plpOffset
    , "limit"    .= plpLimit
    , "total"    .= plpTotal ]

instance FromJSON ProjectsListResponse where
  parseJSON = withObject "/projects/list response" $ \o -> do
    plpProjects <- o .: "projects"
    plpOffset  <- o .: "offset"
    plpLimit   <- o .: "limit"
    plpTotal   <- o .: "total"
    return ProjectsListResponse{..}

-- | /api/v0/projects/list
-- Return the list of available projects
--
-- # Example
--
-- > {
-- >     "limit": 20,
-- >     "offset": 0,
-- >     "projects": [
-- >         {
-- >             "description": "389 Directory Server is an LDAPv3 compliant server. ...",
-- >             "homepage": "https://www.port389.org/",
-- >             "name": "389-ds-base",
-- >             "summary": "389 Directory Server (base)",
-- >             "upstream_vcs": "UPSTREAM_VCS"
-- >         },
-- >         }
-- >     ],
-- >     "total": 2117
-- > }
projectsList :: ServerConfig -> Maybe Int -> Maybe Int -> Handler ProjectsListResponse
projectsList ServerConfig{..} moffset mlimit = do
    result <- runExceptT $ runSqlPool projects cfgPool
    case result of
        -- TODO Properly report errors with a different response
        Left _         -> return $ ProjectsListResponse [] offset limit 0
        Right project_info -> return $ ProjectsListResponse (applyLimits limit offset project_info) offset limit (length project_info)
  where
    -- | Return the offset or the default
    offset :: Int
    offset = fromMaybe 0 moffset

    -- | Return the limit or the default
    limit :: Int
    limit  = fromMaybe 20 mlimit


-- | The JSON response for /projects/info
data Metadata = Metadata {
    mdKey :: T.Text,
    mdVal :: T.Text
} deriving (Show, Eq)

instance ToJSON Metadata where
    toJSON Metadata{..} = object [
        "key" .= mdKey,
        "val" .= mdVal ]

instance FromJSON Metadata where
    parseJSON = withObject "/projects/info metadata" $ \o ->
        Metadata <$> o .: "key"
                 <*> o .: "val"

data SourceInfo = SourceInfo {
    siLicense :: T.Text,
    siMetadata :: [Metadata],
    siSourceRef :: T.Text,
    siVersion :: T.Text
} deriving (Show, Eq)

instance ToJSON SourceInfo where
    toJSON SourceInfo{..} = object [
        "license"    .= siLicense,
        "metadata"   .= siMetadata,
        "source_ref" .= siSourceRef,
        "version"    .= siVersion ]

instance FromJSON SourceInfo where
    parseJSON = withObject "/projects/info source info" $ \o ->
        SourceInfo <$> o .: "license"
                   <*> o .: "metadata"
                   <*> o .: "source_ref"
                   <*> o .: "version"

data BuildInfo = BuildInfo {
    biArch :: T.Text,
    biConfigRef :: T.Text,
    biEnvRef :: T.Text,
    biBuildTime :: UTCTime,
    biChangelog :: T.Text,
    biEpoch :: Maybe Int,
    biMetadata :: [Metadata],
    biRelease :: T.Text,
    biSource :: SourceInfo
} deriving (Show, Eq)

instance ToJSON BuildInfo where
    toJSON BuildInfo{..} = object [
        "arch"             .= biArch,
        "build_config_ref" .= biConfigRef,
        "build_env_ref"    .= biEnvRef,
        "build_time"       .= biBuildTime,
        "changelog"        .= biChangelog,
        "epoch"            .= biEpoch,
        "metadata"         .= biMetadata,
        "release"          .= biRelease,
        "source"           .= biSource ]

instance FromJSON BuildInfo where
    parseJSON = withObject "/projects/info build info" $ \o ->
        BuildInfo <$> o .: "arch"
                  <*> o .: "build_config_ref"
                  <*> o .: "build_env_ref"
                  <*> o .: "build_time"
                  <*> o .: "changelog"
                  <*> o .: "epoch"
                  <*> o .: "metadata"
                  <*> o .: "release"
                  <*> o .: "source"

data ProjectInfo = ProjectInfo {
    piBuilds  :: [BuildInfo],
    piDescription :: T.Text,
    piHomepage :: Maybe T.Text,
    piName :: T.Text,
    piSummary :: T.Text,
    piUpstream :: T.Text
} deriving (Show, Eq)

instance ToJSON ProjectInfo where
    toJSON ProjectInfo{..} = object [
        "builds"       .= piBuilds,
        "description"  .= piDescription,
        "homepage"     .= piHomepage,
        "name"         .= piName,
        "summary"      .= piSummary,
        "upstream_vcs" .= piUpstream ]

instance FromJSON ProjectInfo where
    parseJSON = withObject "/projects/info project info" $ \o ->
        ProjectInfo <$> o .: "builds"
                    <*> o .: "description"
                    <*> o .: "homepage"
                    <*> o .: "name"
                    <*> o .: "summary"
                    <*> o .: "upstream_vcs"

data ProjectsInfoResponse = ProjectsInfoResponse {
    pipProjects :: [ProjectInfo]
} deriving (Show, Eq)

instance ToJSON ProjectsInfoResponse where
    toJSON ProjectsInfoResponse{..} = object [
        "projects" .= pipProjects ]

instance FromJSON ProjectsInfoResponse where
  parseJSON = withObject "/projects/info response" $ \o ->
      ProjectsInfoResponse <$> o .: "projects"

-- | /api/v0/projects/info/<projects>
-- Return information about the comma-separated list of projects
--
-- # Example
--
-- > {
-- >   "projects": [
-- >     {
-- >       "builds": [
-- >         {
-- >           "arch": "x86_64",
-- >           "build_config_ref": "BUILD_CONFIG_REF",
-- >           "build_env_ref": "BUILD_ENV_REF",
-- >           "build_time": "2017-03-01T08:39:23",
-- >           "changelog": "- restore incremental backups correctly, files ...",
-- >           "epoch": "2",
-- >           "metadata": {},
-- >           "release": "32.el7",
-- >           "source": {
-- >             "license": "GPLv3+",
-- >             "metadata": {},
-- >             "source_ref": "SOURCE_REF",
-- >             "version": "1.26"
-- >           }
-- >         }
-- >       ],
-- >       "description": "The GNU tar program saves many files ...",
-- >       "homepage": "http://www.gnu.org/software/tar/",
-- >       "name": "tar",
-- >       "summary": "A GNU file archiving program",
-- >       "upstream_vcs": "UPSTREAM_VCS"
-- >     }
-- >   ]
-- > }
-- >
projectsInfo :: ServerConfig -> String -> Handler ProjectsInfoResponse
projectsInfo ServerConfig{..} project_names = do
    let project_name_list = map T.pack $ sortBy caseInsensitive $ argify [project_names]
    results <- liftIO $ mapM (runExceptT . getProjectInfo) project_name_list
    return $ ProjectsInfoResponse (rights results)
  where
    getProjectInfo :: IsString e => T.Text -> ExceptT e IO ProjectInfo
    getProjectInfo project_name = do
        (projKey, proj) <- fetchProjects project_name
        sources         <- fetchSources projKey
        tuples          <- mapM combineSourceAndBuilds sources

        let nfos = concatMap (\(src, blds) -> map (mkBuildInfo src) blds) tuples

        return ProjectInfo { piBuilds=nfos,
                             piDescription=projectsDescription proj,
                             piHomepage=projectsHomepage proj,
                             piName=projectsName proj,
                             piSummary=projectsSummary proj,
                             piUpstream=projectsUpstream_vcs proj }
     where
        combineSourceAndBuilds :: (Key Sources, Sources) -> ExceptT e IO (Sources, [Builds])
        combineSourceAndBuilds (key, src) = do
            builds <- fetchBuilds key
            return (src, builds)

    mkBuildInfo :: Sources -> Builds -> BuildInfo
    mkBuildInfo src Builds{..} =
        BuildInfo { biArch=buildsArch,
                    biConfigRef=buildsBuild_config_ref,
                    biEnvRef=buildsBuild_env_ref,
                    biBuildTime=buildsBuild_time,
                    biChangelog=cs buildsChangelog,
                    biEpoch=if buildsEpoch == 0 then Nothing else Just buildsEpoch,
                    biMetadata=[],
                    biRelease=buildsRelease,
                    biSource=mkSourceInfo src }

    mkSourceInfo :: Sources -> SourceInfo
    mkSourceInfo Sources{..} =
        SourceInfo { siLicense=sourcesLicense,
                     siMetadata=[],
                     siSourceRef=sourcesSource_ref,
                     siVersion=sourcesVersion }

    fetchProjects :: IsString e => T.Text -> ExceptT e IO (Key Projects, Projects)
    fetchProjects project_name = flip runSqlPool cfgPool $ do
        key  <- findProject project_name >>= maybeToEither "no project record with given name"
        proj <- getProject key >>= maybeToEither "no project record with given name"
        return (key, proj)

    fetchSources :: Key Projects -> ExceptT e IO [(Key Sources, Sources)]
    fetchSources projectId = flip runSqlPool cfgPool $ do
        keys    <- findSources projectId
        sources <- mapM getSource keys
        return $ mapMaybe removeEmptySource (zip keys sources)
     where
        removeEmptySource :: (Key Sources, Maybe Sources) -> Maybe (Key Sources, Sources)
        removeEmptySource (_, Nothing)    = Nothing
        removeEmptySource (key, Just src) = Just (key, src)

    fetchBuilds :: Key Sources -> ExceptT e IO [Builds]
    fetchBuilds sourceId = flip runSqlPool cfgPool $
        findBuilds sourceId >>= mapMaybeM getBuild

-- | The JSON response for /projects/depsolve/<projects>
data ProjectsDepsolveResponse = ProjectsDepsolveResponse {
    pdrProjects  :: [PackageNEVRA]                                      -- ^List of dependencies
} deriving (Show, Eq)

instance ToJSON ProjectsDepsolveResponse where
  toJSON ProjectsDepsolveResponse{..} = object [
      "projects" .= pdrProjects ]

instance FromJSON ProjectsDepsolveResponse where
  parseJSON = withObject "/projects/depsolve response" $ \o -> do
    pdrProjects <- o .: "projects"
    return ProjectsDepsolveResponse{..}

-- | /api/v0/projects/depsolve/<projects>
-- Return the dependencies of a comma separated list of projects
projectsDepsolve :: ServerConfig -> String -> Handler ProjectsDepsolveResponse
projectsDepsolve ServerConfig{..} project_names = do
        let project_name_list = map T.pack (argify [project_names])
        liftIO $ depsolveProjects cfgPool project_name_list >>= \case
            Left _             -> return $ ProjectsDepsolveResponse []
            Right project_deps -> return $ ProjectsDepsolveResponse project_deps


-- | Information about a module
data ModuleName = ModuleName {
    mnName      :: T.Text,                                       -- ^ Module name
    mnGroupType :: T.Text                                        -- ^ Group type (always "rpm" for now)
} deriving (Show, Eq)

instance ToJSON ModuleName where
  toJSON ModuleName{..} = object [
      "name"       .= mnName,
      "group_type" .= mnGroupType ]

instance FromJSON ModuleName where
  parseJSON = withObject "module info" $ \o -> do
    mnName      <- o .: "name"
    mnGroupType <- o .: "group_type"
    return ModuleName{..}

-- | Make a ModuleName from a string
mkModuleName :: T.Text -> ModuleName
mkModuleName name = ModuleName { mnName=name, mnGroupType="rpm" }


-- | The JSON response for /modules/list
data ModulesListResponse = ModulesListResponse {
    mlrModules  :: [ModuleName],                                -- ^ List of modules
    mlrOffset   :: Int,                                         -- ^ Pagination offset into results
    mlrLimit    :: Int,                                         -- ^ Pagination limit of results
    mlrTotal    :: Int                                          -- ^ Total number of module names
} deriving (Show, Eq)

instance ToJSON ModulesListResponse where
  toJSON ModulesListResponse{..} = object [
      "modules" .= mlrModules
    , "offset"  .= mlrOffset
    , "limit"   .= mlrLimit
    , "total"   .= mlrTotal ]

instance FromJSON ModulesListResponse where
  parseJSON = withObject "/modules/list response" $ \o -> do
    mlrModules <- o .: "modules"
    mlrOffset  <- o .: "offset"
    mlrLimit   <- o .: "limit"
    mlrTotal   <- o .: "total"
    return ModulesListResponse{..}

-- | /api/v0/modules/list
-- /api/v0/modules/list/<module_names>
-- Return a list of all of the available modules, filtering by module_names (a comma-separated
-- list).  This includes the name and the group_type, which is currently always "rpm".
--
-- >  {
-- >      "modules": [
-- >        {
-- >          "group_type": "rpm",
-- >          "name": "0ad"
-- >        },
-- >        {
-- >          "group_type": "rpm",
-- >          "name": "0ad-data"
-- >        },
-- >        ....
-- >      ],
-- >      "offset": 0,
-- >      "limit": 20,
-- >      "total": 6
-- >  }
modulesList :: ServerConfig -> Maybe Int -> Maybe Int -> String -> Handler ModulesListResponse

-- | Special case for listing all the modules
-- Uses SQL offset, limit, and case-insensitive sorting
modulesList ServerConfig{..} moffset mlimit "*" = do
    result <- runExceptT $ flip runSqlPool cfgPool $ getGroupsLike offset64 limit64 "%"
    case result of
        Left  _                 ->
            return $ ModulesListResponse [] offset limit 0
        Right (tuples, total64) ->
            let names = nubOrd $ map snd tuples
                objs = map mkModuleName names
            in  return $ ModulesListResponse objs offset limit (fromIntegral total64)
  where
    -- | Return the offset or the default
    offset :: Int
    offset = fromMaybe 0 moffset

    -- | Return the limit or the default
    limit :: Int
    limit  = fromMaybe 20 mlimit

    -- | Return the offset or the default
    offset64 :: Maybe Int64
    offset64 = Just $ fromIntegral $ fromMaybe 0 moffset

    -- | Return the limit or the default
    limit64 :: Maybe Int64
    limit64  = Just $ fromIntegral $ fromMaybe 20 mlimit

modulesList ServerConfig{..} moffset mlimit module_names = do
    -- Substitute % for * in the module_names
    let module_names_list = map T.pack $ argify [map (\c -> if c == '*' then '%' else c) module_names]
    result <- runExceptT $ flip runSqlPool cfgPool $ concatMapM (fmap fst . getGroupsLike Nothing Nothing) module_names_list
    case result of
        Left _       -> return $ ModulesListResponse [] offset limit 0
        Right tuples -> let names = nubOrd $ sortBy caseInsensitiveT $ map snd tuples
                            total = length names
                            objs = applyLimits limit offset $ map mkModuleName names
                        in  return $ ModulesListResponse objs offset limit total
  where
    -- | Return the offset or the default
    offset :: Int
    offset = fromMaybe 0 moffset

    -- | Return the limit or the default
    limit :: Int
    limit  = fromMaybe 20 mlimit

data ComposeBody = ComposeBody {
    cbName :: T.Text,                                                   -- ^ Recipe name (from /blueprints/list)
    cbType :: T.Text,                                                   -- ^ Compose type (from /compose/types)
    cbBranch :: Maybe T.Text                                            -- ^ The git branch to use for this blueprint
} deriving (Show, Eq)

instance ToJSON ComposeBody where
    toJSON ComposeBody{..} = object [
        "blueprint_name"   .= cbName
      , "compose_type"  .= cbType
      , "branch"        .= fromMaybe "master" cbBranch ]

instance FromJSON ComposeBody where
    parseJSON = withObject "compose" $ \o -> do
        cbName   <- o .:  "blueprint_name"
        cbType   <- o .:  "compose_type"
        cbBranch <- o .:? "branch"
        return ComposeBody{..}

-- | JSON status response
data ComposeResponse = ComposeResponse {
    crStatus :: Bool,                                                   -- ^ Success/Failure of the request
    crBuildIDs :: [T.Text],                                             -- ^ UUID of the in-progress build
    crErrors :: [T.Text]                                                -- ^ Errors
} deriving (Show, Eq)

instance ToJSON ComposeResponse where
  toJSON ComposeResponse{..} = object [
      "status"    .= crStatus
    , "build_ids" .= crBuildIDs
    , "errors"    .= crErrors ]

instance FromJSON ComposeResponse where
  parseJSON = withObject "/compose response" $ \o -> do
    crStatus   <- o .: "status"
    crBuildIDs <- o .: "build_ids"
    crErrors   <- o .: "errors"
    return ComposeResponse{..}


-- | POST /api/v0/compose
-- Start a compose.
compose :: ServerConfig -> ComposeBody -> Maybe Int -> Handler ComposeResponse
compose cfg@ServerConfig{..} ComposeBody{..} test | cbType `notElem` supportedOutputs =
    return $ ComposeResponse False [] [T.concat ["Invalid compose type (", cs cbType, "), must be one of ", T.intercalate "," supportedOutputs]]
                                           | otherwise =
    withRecipe cfgRepoLock cbBranch cbName $ \recipe -> do
        buildId <- liftIO nextRandom
        let resultsDir = cfgResultsDir </> show buildId
        liftIO $ do
            createDirectoryIfMissing True resultsDir
            TIO.writeFile (resultsDir </> "STATUS") (queueStatusText QWaiting)
            -- Write out the original recipe.
            TIO.writeFile (resultsDir </> "blueprint.toml") (recipeTOML recipe)

        -- Freeze the recipe so we have precise versions of its components.  This could potentially
        -- return multiple frozen recipes, but I think only if we asked it to do multiple things.
        -- We did not, so we can safely assume there's only one result.
        withFrozenRecipe cbBranch cbName $ \frozen -> liftIO $ do
            TIO.writeFile (resultsDir </> "frozen.toml") (recipeTOML frozen)

            customActions <- processCustomization $ rCustomization frozen

            let dest = resultsDir </> "compose." ++ T.unpack cbType
                ci   = ComposeInfo { ciDest=dest,
                                     ciId=T.pack $ show buildId,
                                     ciRecipe=recipe,
                                     ciResultsDir=resultsDir,
                                     ciCustom=customActions,
                                     ciType=cbType }

            void $ atomicModifyIORef' cfgWorkQ (\ref -> (ref ++ [ci], ()))
            return $ ComposeResponse True [T.pack $ show buildId] []
 where
    withRecipe :: GitLock -> Maybe T.Text -> T.Text -> (Recipe -> Handler ComposeResponse) -> Handler ComposeResponse
    withRecipe lock branch name fn =
        liftIO (getRecipeInfo lock (defaultBranch $ fmap cs branch) name) >>= \case
            Left err          -> return $ ComposeResponse False [] [T.pack err]
            Right (_, recipe) -> fn recipe

    withFrozenRecipe :: Maybe T.Text -> T.Text -> (Recipe -> Handler ComposeResponse) -> Handler ComposeResponse
    withFrozenRecipe branch name fn =
        recipesFreeze cfg (fmap cs branch) (cs name) >>= \case
            RecipesFreezeResponse [] errs      -> return $ ComposeResponse False [] (map (T.pack . show) errs)
            RecipesFreezeResponse (frozen:_) _ -> fn frozen

-- | The JSON response for /compose/types
data ComposeType = ComposeType {
    ctEnabled :: Bool,                      -- ^ Is this output type enabled?
    ctName    :: T.Text                     -- ^ The name of the output type
} deriving (Show, Eq)

instance ToJSON ComposeType where
    toJSON ComposeType{..} = object [
        "enabled" .= ctEnabled
      , "name"    .= ctName ]

instance FromJSON ComposeType where
    parseJSON = withObject "compose type" $ \o -> do
        ctEnabled <- o .: "enabled"
        ctName    <- o .: "name"
        return ComposeType{..}

data ComposeTypesResponse = ComposeTypesResponse {
    ctrTypes :: [ComposeType]
} deriving (Show, Eq)

instance ToJSON ComposeTypesResponse where
  toJSON ComposeTypesResponse{..} = object [
      "types" .= ctrTypes ]

instance FromJSON ComposeTypesResponse where
  parseJSON = withObject "/compose/types response" $ \o -> do
      ctrTypes <- o .: "types"
      return ComposeTypesResponse{..}


-- | /api/v0/compose/types
--
-- Returns the list of supported output types that are valid for use with 'POST /api/v0/compose'
--
-- > {
-- >   "types": [
-- >     {
-- >       "enabled": true,
-- >       "name": "tar"
-- >     }
-- >   ]
-- > }
composeTypes :: Handler ComposeTypesResponse
composeTypes =
    return $ ComposeTypesResponse $ map (ComposeType True) supportedOutputs


data ComposeQueueResponse = ComposeQueueResponse {
    cqrNew :: [ComposeStatus],
    cqrRun :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeQueueResponse where
  toJSON ComposeQueueResponse{..} = object [
      "new" .= cqrNew
    , "run" .= cqrRun ]

instance FromJSON ComposeQueueResponse where
  parseJSON = withObject "/compose/queue response" $ \o ->
      ComposeQueueResponse <$> o .: "new"
                           <*> o .: "run"

-- | /api/v0/compose/queue
--
-- Return the status of the build queue. It includes information about the builds waiting,
-- and the build that is running.
-- 
-- > {
-- >   "new": [
-- >     {
-- >       "id": "45502a6d-06e8-48a5-a215-2b4174b3614b",
-- >       "recipe": "glusterfs",
-- >       "queue_status": "WAITING",
-- >       "timestamp": 1517362647.4570868,
-- >       "version": "0.0.6"
-- >     },
-- >     {
-- >       "id": "6d292bd0-bec7-4825-8d7d-41ef9c3e4b73",
-- >       "recipe": "kubernetes",
-- >       "queue_status": "WAITING",
-- >       "timestamp": 1517362659.0034983,
-- >       "version": "0.0.1"
-- >     }
-- >   ],
-- >   "run": [
-- >     {
-- >       "id": "745712b2-96db-44c0-8014-fe925c35e795",
-- >       "recipe": "glusterfs",
-- >       "queue_status": "RUNNING",
-- >       "timestamp": 1517362633.7965999,
-- >       "version": "0.0.6"
-- >     }
-- >   ]
-- > }
composeQueue :: ServerConfig -> Handler ComposeQueueResponse
composeQueue ServerConfig{..} = do
    -- Construct a new message to ask what composes are currently waiting.
    -- Each message includes an initially empty TMVar where the response
    -- will be written.  This prevents needing to write a communications
    -- protocol.  Making it initially empty is very important.
    r <- liftIO $ atomically newEmptyTMVar
    liftIO $ atomically $ writeTChan cfgChan (AskBuildsWaiting, r)

    -- Wait for the response to show up in the TMVar we created.  This blocks,
    -- but the server doesn't do much in its main thread so it shouldn't block
    -- for long.
    buildsWaiting <- liftIO (atomically $ readTMVar r) >>= \case
        RespBuildsWaiting lst -> return lst
        _                     -> return []

    -- And then we do the same thing for builds currently running.
    r' <- liftIO $ atomically newEmptyTMVar
    liftIO $ atomically $ writeTChan cfgChan (AskBuildsInProgress, r')
    buildsRunning <- liftIO (atomically $ readTMVar r') >>= \case
        RespBuildsInProgress lst -> return lst
        _                        -> return []

    -- Finally we can create a response to send back to the client.
    waitingCS <- filterMapComposeStatus cfgResultsDir buildsWaiting
    runningCS <- filterMapComposeStatus cfgResultsDir buildsRunning
    return $ ComposeQueueResponse waitingCS runningCS


data ComposeFinishedResponse = ComposeFinishedResponse {
    cfrFinished :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeFinishedResponse where
  toJSON ComposeFinishedResponse{..} = object [
      "finished" .= cfrFinished ]

instance FromJSON ComposeFinishedResponse where
  parseJSON = withObject "/compose/queue/finished response" $ \o ->
      ComposeFinishedResponse <$> o .: "finished"

-- | /api/v0/compose/finished
--
-- Return the details on all of the finished composes on the system.
--
-- > {
-- >   "finished": [
-- >     {
-- >       "id": "70b84195-9817-4b8a-af92-45e380f39894",
-- >       "recipe": "glusterfs",
-- >       "queue_status": "FINISHED",
-- >       "timestamp": 1517351003.8210032,
-- >       "version": "0.0.6"
-- >     },
-- >     {
-- >       "id": "e695affd-397f-4af9-9022-add2636e7459",
-- >       "recipe": "glusterfs",
-- >       "queue_status": "FINISHED",
-- >       "timestamp": 1517362289.7193348,
-- >       "version": "0.0.6"
-- >     }
-- >   ]
-- > }
composeQueueFinished :: ServerConfig -> Handler ComposeFinishedResponse
composeQueueFinished ServerConfig{..} = do
    results <- liftIO $ getComposesWithStatus cfgResultsDir QFinished
    return $ ComposeFinishedResponse results


data ComposeFailedResponse = ComposeFailedResponse {
    cfrFailed :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeFailedResponse where
  toJSON ComposeFailedResponse{..} = object [
      "failed" .= cfrFailed ]

instance FromJSON ComposeFailedResponse where
  parseJSON = withObject "/compose/queue/failed response" $ \o ->
      ComposeFailedResponse <$> o .: "failed"

-- | /api/v0/compose/failed
--
-- Return the details on all of the failed composes on the system.
--
-- > {
-- >   "failed": [
-- >     {
-- >       "id": "8c8435ef-d6bd-4c68-9bf1-a2ef832e6b1a",
-- >       "recipe": "http-server",
-- >       "queue_status": "FAILED",
-- >       "timestamp": 1517523249.9301329,
-- >       "version": "0.0.2"
-- >     }
-- >   ]
-- > }
composeQueueFailed :: ServerConfig -> Handler ComposeFailedResponse
composeQueueFailed ServerConfig{..} = do
    results <- liftIO $ getComposesWithStatus cfgResultsDir QFailed
    return $ ComposeFailedResponse results


data ComposeStatusResponse = ComposeStatusResponse {
    csrUuids :: [ComposeStatus]
} deriving (Show, Eq)

instance ToJSON ComposeStatusResponse where
  toJSON ComposeStatusResponse{..} = object [
      "uuids" .= csrUuids ]

instance FromJSON ComposeStatusResponse where
  parseJSON = withObject "/compose/queue/status response" $ \o ->
      ComposeStatusResponse <$> o .: "uuids"

-- | /api/v0/compose/status/<uuids>
--
-- Return the details for each of the comma-separated list of uuids.
--
-- > {
-- >   "uuids": [
-- >     {
-- >       "id": "8c8435ef-d6bd-4c68-9bf1-a2ef832e6b1a",
-- >       "recipe": "http-server",
-- >       "queue_status": "FINISHED",
-- >       "timestamp": 1517523644.2384307,
-- >       "version": "0.0.2"
-- >     },
-- >     {
-- >       "id": "45502a6d-06e8-48a5-a215-2b4174b3614b",
-- >       "recipe": "glusterfs",
-- >       "queue_status": "FINISHED",
-- >       "timestamp": 1517363442.188399,
-- >       "version": "0.0.6"
-- >     }
-- >   ]
-- > }
composeStatus :: ServerConfig -> [T.Text] -> Handler ComposeStatusResponse
composeStatus ServerConfig{..} uuids =
    ComposeStatusResponse <$> filterMapComposeStatus cfgResultsDir uuids


data ComposeDeleteResponse = ComposeDeleteResponse {
    cdrErrors :: [UuidError],
    cdrUuids :: [UuidStatus]
} deriving (Show, Eq)

instance ToJSON ComposeDeleteResponse where
    toJSON ComposeDeleteResponse{..} = object [
        "errors" .= cdrErrors,
        "uuids"  .= cdrUuids ]

instance FromJSON ComposeDeleteResponse where
    parseJSON = withObject "/compose/delete response" $ \o ->
        ComposeDeleteResponse <$> o .: "cdrErrors"
                              <*> o .: "cdrUuids"

-- | DELETE /api/v0/compose/delete/<uuids>
--
-- Delete the list of comma-separated uuids from the compose results.
--
-- > {
-- >   "errors": [],
-- >   "uuids": [
-- >     {
-- >       "status": true,
-- >       "uuid": "ae1bf7e3-7f16-4c9f-b36e-3726a1093fd0"
-- >     }
-- >   ]
-- > }
composeDelete :: ServerConfig -> [T.Text] -> Handler ComposeDeleteResponse
composeDelete ServerConfig{..} uuids = do
    results <- liftIO $ mapM (deleteCompose cfgResultsDir) uuids
    let (errors, successes) = partitionEithers results
    return ComposeDeleteResponse { cdrErrors=errors, cdrUuids=successes }


-- | /api/v0/compose/logs/<uuid>
--
-- Returns a .tar of the compose logs.  The tar is not compressed, but it is
-- not large.
--
-- The mime type is set to 'application/x-tar' and the filename is set to
-- UUID-logs.tar
composeLogs :: KnownSymbol h => ServerConfig -> String -> Handler (Headers '[Header h String] LBS.ByteString)
composeLogs ServerConfig{..} uuid = do
    result <- liftIO $ runExceptT $ mkComposeStatus cfgResultsDir (cs uuid)
    case result of
        Left _                  -> throwError $ createApiError err400 "compose_logs" (cs uuid ++ " is not a valid build uuid")
        Right ComposeStatus{..} ->
            if not (queueStatusEnded csQueueStatus)
            then throwError $ createApiError err400 "compose_logs" ("Build " ++ cs uuid ++ " not in FINISHED or FAILED state.")
            else do
                let composeResultsDir = cfgResultsDir </> cs uuid
                    logFiles          = ["compose.log"]

                tar <- liftIO $ Tar.pack composeResultsDir logFiles
                return $ addHeader ("attachment; filename=" ++ uuid ++ "-logs.tar;") (Tar.write tar)
