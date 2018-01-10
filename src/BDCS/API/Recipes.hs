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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| Git Recipe storage functions

    Recipes are stored in a bare git repository. The repository is created with
    'openOrCreateRepo' which returns the Repository which is passed to all of
    the other functions.

-}
module BDCS.API.Recipes(openOrCreateRepo,
                        findOrCreateBranch,
                        getBranchOIdFromObject,
                        writeCommit,
                        readCommit,
                        readCommitSpec,
                        listBranchFiles,
                        listCommitFiles,
                        deleteFile,
                        deleteRecipe,
                        revertFile,
                        revertFileCommit,
                        revertRecipe,
                        listRecipeCommits,
                        listCommits,
                        findCommitTag,
                        getRevisionFromTag,
                        tagFileCommit,
                        tagRecipeCommit,
                        commitRecipeFile,
                        commitRecipe,
                        commitRecipeDirectory,
                        readRecipeCommit,
                        recipeDiff,
                        runGitRepoTests,
                        runWorkspaceTests,
                        CommitDetails(..),
                        RecipeDiffEntry(..),
                        RecipeDiffType(..),
                        GitError(..),
                        printOId)
  where

import           BDCS.API.Recipe
import           BDCS.API.Utils(caseInsensitive, maybeThrow)
import           BDCS.API.Workspace
import           Control.Conditional(ifM, whenM)
import           Control.Exception
import           Control.Monad(filterM, unless, void)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Loops(allM)
import           Data.Aeson(FromJSON(..), ToJSON(..), (.=), (.:), object, withObject, Value(..))
import qualified Data.ByteString as BS
import           Data.Either(rights)
import           Data.Foldable(asum)
import           Data.List(elemIndices, find, isSuffixOf, sortBy)
import           Data.Maybe(fromJust, isJust)
import           Data.Set(difference, fromList, intersection, Set, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Encoding(decodeUtf8, encodeUtf8)
import           Data.Word(Word32)
import           GI.Gio
import qualified GI.Ggit as Git
import qualified GI.GLib as GLib
import           System.Directory(doesFileExist, doesPathExist, listDirectory)
import           System.FilePath.Posix((</>))
import           System.IO.Temp(withTempDirectory)
import           Text.Printf(printf)
import           Text.Read(readMaybe)


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | Errors that can be thrown by the BDCS.API.Recipes functions.
data GitError =
    OpenRepoError                               -- ^ Repo open error
  | CreateRepoError                             -- ^ Problem creating a new repo
  | CreateBlobError                             -- ^ New Blob error
  | CreateCommitError                           -- ^ Error creating a commit
  | CreateBranchError                           -- ^ New Branch error
  | BranchNameError                             -- ^ Branch name error, eg. doesn't exist
  | WriteTreeError                              -- ^ Tree writing error
  | GetIndexError                               -- ^ Error getting the repository error
  | GetHeadError                                -- ^ Error getting the repository head
  | RefLookupError                              -- ^ Error looking up a ref. eg. doesn't exist
  | TreeBuilderError                            -- ^ Problem creating a Tree Builder for a Tree.
  | GetByNameError                              -- ^ Problem getting a Tree by name
  | GetNameError                                -- ^ Problem getting a Tree Entry by name
  | GetTargetError                              -- ^ Error getting ref. target
  | GetTimeError                                -- ^ Problem getting the time from the Signature
  | GetTimeZoneError                            -- ^ Problem getting the timezone from the Signature
  | GetTreeError                                -- ^ Error getting Commit Tree
  | GetTreeIdError                              -- ^ Error getting commit Tree Id
  | GetCommitterError                           -- ^ Error getting the committer's Signature
  | GetMessageError                             -- ^ Error getting commit message
  | GetParentsError                             -- ^ Problem getting commit's parents
  | LookupError                                 -- ^ Error looking up a commit
  | LookupBlobError                             -- ^ Error looking up a Blob OId
  | LookupBranchError                           -- ^ Branch error, eg. doesn't exist
  | LookupCommitError                           -- ^ Commit error, eg. commit doesn't exist
  | LookupTagError                              -- ^ Error looking up a Tag. eg. doesn't exist
  | LookupTreeError                             -- ^ Tree Lookup error. eg. tree id doesn't exist
  | LookupReferenceError                        -- ^ Problem looking up a reference
  | RevparseError                               -- ^ Problem parsing a revision spec
  | BuilderWriteError                           -- ^ Tree Builder write error
  | BuilderInsertError                          -- ^ Tree Builder insert error
  | GetEntryIdError                             -- ^ Error getting a tree entry id
  | GetIdError                                  -- ^ Problem getting object's id
  | GetRawBlobError                             -- ^ Error getting the raw Blob content
  | GetTargetIdError                            -- ^ Error getting Tag Id from a tag object
  | NewOIdError                                 -- ^ Problem creating a new OId from a string
  | NewOptionsError                             -- ^ Error creating a new Options object
  | NewTimeValError                             -- ^ Error creating a new TimeVal object
  | NewTreeError                                -- ^ Problem creating a new diff Tree
  | NewSignatureError                           -- ^ Error creating a new Signature
  | NewWalkerError                              -- ^ Error creating a new revision Walker object
  | OIdError                                    -- ^ Error creating a String from an OId
  deriving (Eq, Show)

instance Exception GitError

-- | Get the branch's HEAD Commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
--
-- Can throw 'LookupBranchError' or 'LookupCommitError'
headCommit :: Git.Repository -> T.Text -> IO Git.Commit
headCommit repo branch = do
    branch_obj <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal >>= maybeThrow LookupBranchError
    branch_id <- getBranchOIdFromObject repo branch_obj
    Git.repositoryLookupCommit repo branch_id >>= maybeThrow LookupCommitError

-- | Prepare for a commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@builder@]: Tree to add the commit to
--
-- Returns a tuple of information used when making a commit
--
-- Can throw 'BuilderWriteError', 'LookupTreeError', 'NewSignatureError'
prepareCommit :: Git.Repository -> T.Text -> Git.TreeBuilder -> IO (Git.Tree, Git.Signature, Maybe T.Text, Maybe T.Text)
prepareCommit repo branch builder = do
    tree_id <- Git.treeBuilderWrite builder >>= maybeThrow BuilderWriteError
    tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError
    sig <- Git.signatureNewNow "bdcs-api" "user-email" >>= maybeThrow NewSignatureError
    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"
    return (tree, sig, ref, encoding)

-- | Open a Git repository, or create the initial repository if one doesn't exist
--
-- [@path@]: Path to the git repository
--
-- The bare git repository is created in ./git underneath path
-- If the directory doesn't look like an existing git repo (no ./git/HEAD file) then a new
-- bare repository is created.
--
-- Can throw 'OpenRepoError', 'CreateRepoError', 'NewSignatureError', 'GetIndexError',
-- 'WriteTreeError', 'LookupTreeError', or 'CreateCommitError'
openOrCreateRepo :: FilePath -> IO Git.Repository
openOrCreateRepo path = do
    gfile <- fileNewForPath (path ++ "/git")
    ifM (doesPathExist $ path ++ "/git/HEAD")
        (openRepo gfile)
        (createWithInitialCommit gfile)
  where
    openRepo gfile = Git.repositoryOpen gfile >>= maybeThrow OpenRepoError

    createWithInitialCommit gfile = do
        repo <- Git.repositoryInitRepository gfile True >>= maybeThrow CreateRepoError

        -- Make an empty initial commit
        sig <- Git.signatureNewNow "bdcs-api" "user-email" >>= maybeThrow NewSignatureError
        index <- Git.repositoryGetIndex repo >>= maybeThrow GetIndexError
        tree_id <- Git.indexWriteTree index >>= maybeThrow WriteTreeError
        tree <- Git.repositoryLookupTree repo tree_id >>= maybeThrow LookupTreeError
        let ref = Just "HEAD"
        let encoding = Just "UTF-8"
        void $ Git.repositoryCreateCommit repo ref sig sig encoding "Initial Recipe repository commit" tree [] >>= maybeThrow CreateCommitError

        return repo

-- | Lookup the Branch name or create a new branch and return a Git.Branch
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
--
-- Can throw 'GetHeadError', 'RefLookupError', or 'CreateBranchError'
findOrCreateBranch :: Git.Repository -> T.Text -> IO Git.Branch
findOrCreateBranch repo branch = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    maybe createBranch return mbranch
  where
    createBranch = do
        head_ref <- Git.repositoryGetHead repo >>= maybeThrow GetHeadError
        parent_obj <- Git.refLookup head_ref >>= maybeThrow RefLookupError
        Git.repositoryCreateBranch repo branch parent_obj [Git.CreateFlagsNone] >>= maybeThrow CreateBranchError

-- | Convert a Branch object to an OId
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
--
-- Can throw 'BranchNameError', 'LookupReferenceError', or 'GetTargetError'
getBranchOIdFromObject :: Git.Repository -> Git.Branch -> IO Git.OId
getBranchOIdFromObject repo branch_obj = do
    branch_name <- Git.branchGetName branch_obj >>= maybeThrow BranchNameError
    let branch_ref = T.pack $ printf "refs/heads/%s" branch_name
    ref <- Git.repositoryLookupReference repo branch_ref >>= maybeThrow LookupReferenceError
    Git.refGetTarget ref >>= maybeThrow GetTargetError

-- | Make a new commit to a repository's branch
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: Filename of the commit
-- [@message@]: Commit message
-- [@content@]: Data to be written to the commit
--
-- Returns the OId of the new commit.
--
-- Can throw 'CreateBlobError', 'GetTreeError', 'TreeBuilderError', 'BuilderInsertError', or 'CreateCommitError'
writeCommit :: Git.Repository -> T.Text -> T.Text -> T.Text -> BS.ByteString -> IO Git.OId
writeCommit repo branch filename message content = do
    -- TODO Create the branch if it doesn't already exist (using findOrCreateBranch)
    parent_commit <- headCommit repo branch
    blob_id <- Git.repositoryCreateBlobFromBuffer repo content >>= maybeThrow CreateBlobError

    -- Use treebuilder to make a new entry for this filename and blob: repositoryCreateTreeBuilderFromTree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    void $ Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob >>= maybeThrow BuilderInsertError
    (tree, sig, ref, encoding) <- prepareCommit repo branch builder
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit] >>= maybeThrow CreateCommitError

-- | Read a commit and return a ByteString of the content
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: Filename of the commit
-- [@commit@]: Commit hash to read, or Nothing to read the HEAD
--
-- TODO Return the commit message too
readCommit :: Git.Repository -> T.Text -> T.Text -> Maybe T.Text -> IO BS.ByteString
readCommit repo branch filename Nothing = do
    let spec = T.pack $ printf "%s:%s" branch filename
    readCommitSpec repo spec
readCommit repo _ filename commit = do
    let spec = T.pack $ printf "%s:%s" (fromJust commit) filename
    readCommitSpec repo spec

-- | Read a commit usinga revspec, return the ByteString content
--
-- [@repo@]: Open git repository
-- [@spec@]: revspec to read.
--
-- eg. \<commit\>:\<filename\> or \<branch\>:\<filename\>
--
-- Can throw 'RevparseError', 'GetIdError', 'LookupBlobError', or 'GetRawBlobError'
readCommitSpec :: Git.Repository -> T.Text -> IO BS.ByteString
readCommitSpec repo spec = do
    obj <- Git.repositoryRevparse repo spec >>= maybeThrow RevparseError
    oid <- Git.objectGetId obj >>= maybeThrow GetIdError
    blob <- Git.repositoryLookupBlob repo oid >>= maybeThrow LookupBlobError
    Git.blobGetRawContent blob >>= maybeThrow GetRawBlobError

-- | Get the filename for a Blob tree entry
--
-- [@tree@]: The commit's Tree object
-- [@idx@]: Entry index to get
--
-- Can throw 'GetTreeError', or 'GetNameError'
getFilename :: Git.Tree -> Word32 -> IO (Maybe T.Text)
getFilename tree idx = do
    entry <- Git.treeGet tree idx >>= maybeThrow GetTreeError

    -- Only allow Blob and BlobExecutable
    ifM (isFileBlob entry)
        (Just <$> Git.treeEntryGetName entry >>= maybeThrow GetNameError)
        (return Nothing)
 where
    isFileBlob entry = Git.treeEntryGetFileMode entry >>= \case
        Git.FileModeBlob           -> return True
        Git.FileModeBlobExecutable -> return True
        _                          -> return False

{-# ANN getFilenames ("HLint: ignore Eta reduce"::String) #-}
-- | Get a list of the Blob tree entry filenames
--
-- [@tree@]: The commit's Tree object
-- [@idx@]: Entry index to get
--
-- This is limited to entries of type Blob and BlobExecutable
getFilenames :: Git.Tree -> Word32 -> IO [T.Text]
getFilenames tree idx = getFilenames' tree [] idx

-- | Build the list of filenames from the tree entries
--
-- [@tree@]: The commit's Tree object
-- [@filenames@]: The accumulated list of filenames
-- [@idx@]: Entry index to get
getFilenames' :: Git.Tree -> [T.Text] -> Word32 -> IO [T.Text]
getFilenames' _ filenames 0 = return filenames
getFilenames' tree filenames idx = getFilename tree (idx-1) >>= \case
    Just name -> getFilenames' tree (name:filenames) (idx-1)
    Nothing   -> getFilenames' tree filenames (idx-1)

-- | List the files on a branch
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
listBranchFiles :: Git.Repository -> T.Text -> IO [T.Text]
listBranchFiles repo branch =
    headCommit repo branch >>= listCommitFiles repo

-- | List the files in a commit
--
-- [@repo@]: Open git repository
-- [@commit@]: The commit to get the files from
--
-- Can throw 'GetTreeIdError', or 'LookupTreeError'
listCommitFiles :: Git.Repository -> Git.Commit -> IO [T.Text]
listCommitFiles repo commit = do
    parent_tree_id <- Git.commitGetTreeId commit >>= maybeThrow GetTreeIdError
    tree <- Git.repositoryLookupTree repo parent_tree_id >>= maybeThrow LookupTreeError
    sz <- Git.treeSize tree
    getFilenames tree sz

-- | Delete a recipe from a branch
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@recipe_name@]: The recipe name to delete (not the filename)
deleteRecipe :: Git.Repository -> T.Text -> T.Text -> IO Git.OId
deleteRecipe repo branch recipe_name = deleteFile repo branch (recipeTomlFilename $ T.unpack recipe_name)

-- | Delete a file from a branch
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: The recipe filename to delete
--
-- Can throw `GetTreeError`, 'TreeBuilderError', or 'CreateCommitError'
deleteFile :: Git.Repository -> T.Text -> T.Text -> IO Git.OId
deleteFile repo branch filename = do
    parent_commit <- headCommit repo branch

    -- Use treebuilder to modify the tree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    Git.treeBuilderRemove builder filename
    (tree, sig, ref, encoding) <- prepareCommit repo branch builder
    let message = T.pack $ printf "Recipe %s deleted" filename
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit] >>= maybeThrow  CreateCommitError

{-# ANN revertRecipe ("HLint: ignore Eta reduce"::String) #-}
-- | Revert a recipe to a previous commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@recipe_name@]: The recipe name to revert (not the filename)
-- [@commit@]: The commit hash string to revert to
revertRecipe :: Git.Repository -> T.Text -> T.Text -> T.Text -> IO Git.OId
revertRecipe repo branch recipe_name commit = revertFile repo branch (recipeTomlFilename $ T.unpack recipe_name) commit

-- | Revert a recipe file to a previous commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: The recipe filename to revert
-- [@commit@]: The commit hash string to revert to
--
-- Can throw 'NewOIdError'
revertFile :: Git.Repository -> T.Text -> T.Text -> T.Text -> IO Git.OId
revertFile repo branch filename commit = do
    commit_id <- Git.oIdNewFromString commit >>= maybeThrow NewOIdError
    revertFileCommit repo branch filename commit_id

-- | Revert a recipe file to a previous commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: The recipe filename to revert
-- [@commit@]: The commit object to revert to
--
-- Can throw 'LookupCommitError', 'GetTreeError', 'GetByNameError', 'GetEntryIdError', 'GetTreeError',
-- '', 'OIdError', 'CreateCommitError'
revertFileCommit :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO Git.OId
revertFileCommit repo branch filename commit_id = do
    commit_obj <- Git.repositoryLookupCommit repo commit_id >>= maybeThrow LookupCommitError
    revert_tree <- Git.commitGetTree commit_obj >>= maybeThrow GetTreeError
    entry <- Git.treeGetByName revert_tree filename >>= maybeThrow GetByNameError
    blob_id <- Git.treeEntryGetId entry >>= maybeThrow GetEntryIdError
    parent_commit <- headCommit repo branch

    -- Use treebuilder to modify the tree
    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree >>= maybeThrow TreeBuilderError
    void $ Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob
    (tree, sig, ref, encoding) <- prepareCommit repo branch builder
    commit <- Git.oIdToString commit_id >>= maybeThrow OIdError
    let message = T.pack $ printf "Recipe %s reverted to commit %s" filename commit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit] >>= maybeThrow CreateCommitError

-- | File commit details
data CommitDetails =
    CommitDetails { cdCommit    :: T.Text                       -- ^ Hash string
                  , cdTime      :: T.Text                       -- ^ Timestamp in ISO 8601 format
                  , cdMessage   :: T.Text                       -- ^ Commit message, separated by \n
                  , cdRevision  :: Maybe Int                    -- ^ Recipe revision number
    } deriving (Show, Eq)

-- JSON instances for CommitDetails
instance ToJSON CommitDetails where
  toJSON CommitDetails{..} = object [
      "commit"   .= cdCommit
    , "time"     .= cdTime
    , "message"  .= cdMessage
    , "revision" .= cdRevision ]

instance FromJSON CommitDetails where
  parseJSON = withObject "/recipes/info response" $ \o -> do
    cdCommit   <- o .: "commit"
    cdTime     <- o .: "time"
    cdMessage  <- o .: "message"
    cdRevision <- o .: "revision"
    return CommitDetails{..}

-- | List the commits for a recipe
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@recipe_name@]: Recipe name (not filename)
--
-- Returns a list of 'CommitDetails'
listRecipeCommits :: Git.Repository -> T.Text -> T.Text -> IO [CommitDetails]
listRecipeCommits repo branch recipe_name = listCommits repo branch (recipeTomlFilename $ T.unpack recipe_name)

-- | List the commits for a filename
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: Recipe filename
--
-- Returns a list of 'CommitDetails'
--
-- Can throw 'NewWalkerError'
listCommits :: Git.Repository -> T.Text -> T.Text -> IO [CommitDetails]
listCommits repo branch filename = do
    revwalk <- Git.revisionWalkerNew repo >>= maybeThrow NewWalkerError
    Git.revisionWalkerSetSortMode revwalk [Git.SortModeReverse]
    let branch_ref = T.pack $ printf "refs/heads/%s" branch
    Git.revisionWalkerPushRef revwalk branch_ref

    mfirst_id <- Git.revisionWalkerNext revwalk
    commitDetails repo revwalk branch filename [] mfirst_id

-- | Get the commit details for a filename
--
-- [@repo@]: Open git repository
-- [@revwalk@]: Git revwalk object
-- [@branch@]: Branch name
-- [@filename@]: Recipe filename
-- [@details@]: Accumulated 'CommitDetails' for the filename
-- [@next_id@]: Next commit OId
--
-- This is a recursive function that accumulates the details for the filename,
-- returning when there are no more commits to examine.
--
-- Can throw 'LookupCommitError', 'GetParentsError', 'GetTreeError', 'GetMessageError',
-- 'OIdError', 'GetCommitterError', 'GetTimeError'
commitDetails :: Git.Repository -> Git.RevisionWalker -> T.Text -> T.Text -> [CommitDetails] -> Maybe Git.OId -> IO [CommitDetails]
commitDetails _ _ _ _ details Nothing = return details
commitDetails repo revwalk branch filename details next_id = do
    let commit_id = fromJust next_id
    commit_obj <- Git.repositoryLookupCommit repo commit_id >>= maybeThrow LookupCommitError

    parents <- Git.commitGetParents commit_obj >>= maybeThrow GetParentsError
    num_parents <- Git.commitParentsGetSize parents

    tree <- Git.commitGetTree commit_obj >>= maybeThrow GetTreeError

    is_diff <- if num_parents > 0
        then do
            commits <- mapM (getCommitParent parents) [0..num_parents-1]
            allM (parentDiff repo filename tree) commits
        else
            return False

    mnext_id <- Git.revisionWalkerNext revwalk
    mentry <- Git.treeGetByName tree filename
    if isJust mentry && is_diff
        then getCommitDetails commit_id commit_obj mnext_id
        else commitDetails repo revwalk branch filename details mnext_id

  where
    getCommitParent :: Git.CommitParents -> Word32 -> IO Git.Commit
    getCommitParent parents idx = Git.commitParentsGet parents idx >>= maybeThrow GetParentsError

    getCommitDetails :: Git.OId -> Git.Commit -> Maybe Git.OId -> IO [CommitDetails]
    getCommitDetails commit_id commit_obj mnext_id = do
        mtag <- findCommitTag repo branch filename commit_id
        let revision = getRevisionFromTag mtag
        -- Fill in a commit record
        message <- Git.commitGetMessage commit_obj >>= maybeThrow GetMessageError
        commit_str <- Git.oIdToString commit_id >>= maybeThrow OIdError
        sig <- Git.commitGetCommitter commit_obj >>= maybeThrow GetCommitterError

        time_str <- Git.signatureGetTime sig >>= maybeThrow GetTimeError >>= formatDateTime

        let commit = CommitDetails {cdCommit=commit_str, cdTime=time_str, cdMessage=message, cdRevision=revision}
        commitDetails repo revwalk branch filename (commit:details) mnext_id

    formatDateTime :: MonadIO m => GLib.DateTime -> m T.Text
    formatDateTime datetime = do
        -- convert the datetime to UTC
        utctime <- GLib.dateTimeToUtc datetime

        -- Here are two other obvious ways of
        -- Pull the values out of the datetime directly instead of converting
        -- to a timeval, because
        --   1) converting to/from a timeval can fail (!!)
        --   2) the annotations for g_date_time_to_timeval are busted so
        --      the binding ends up relying on side effects
        --
        -- g_date_time_format doesn't cut it either, since it can't print microseconds
        year   <- GLib.dateTimeGetYear        utctime
        month  <- GLib.dateTimeGetMonth       utctime
        day    <- GLib.dateTimeGetDayOfMonth  utctime
        hour   <- GLib.dateTimeGetHour        utctime
        minute <- GLib.dateTimeGetMinute      utctime
        second <- GLib.dateTimeGetSecond      utctime
        micro  <- GLib.dateTimeGetMicrosecond utctime

        -- Print it out in the same format as g_time_val_to_iso8601
        let secondsStr = (if (micro /= 0) then printf "%02d.%06d" second micro
                                         else printf "%02d" second) :: String

        return $ T.pack $ printf "%d-%02d-%02dT%02d:%02d:%sZ" year month day hour minute secondsStr

-- | Determine if there were changes between a file's commit and its parent
--
-- [@repo@]: Open git repository
-- [@filename@]: Filename to check
-- [@commit_tree@]: The filename's commit Tree
-- [@parent_commit@]: The parent commit to check
--
-- Return True if there were changes, False otherwise
parentDiff :: Git.Repository -> T.Text -> Git.Tree -> Git.Commit -> IO Bool
parentDiff repo filename commit_tree parent_commit = do
    diff_opts <- Git.diffOptionsNew >>= maybeThrow NewOptionsError
    Git.diffOptionsSetPathspec diff_opts (Just [filename])

    parent_tree <- Git.commitGetTree parent_commit >>= maybeThrow GetTreeError
    diff <- Git.diffNewTreeToTree repo (Just commit_tree) (Just parent_tree) (Just diff_opts) >>= maybeThrow NewTreeError
    num_deltas <- Git.diffGetNumDeltas diff
    return $ num_deltas > 0

-- | Find the revision tag pointing to a specific commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: Recipe filename
-- [@commit_id@]: The commit OId
--
-- The Tag is of the form refs/tags/\<branch\>/\<filename\>/r\<revision\>
-- There should only be one result.
findCommitTag :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO (Maybe T.Text)
findCommitTag repo branch filename commit_id = do
    let tag_pattern = T.pack $ printf "%s/%s/r*" branch filename
    Git.repositoryListTagsMatch repo (Just tag_pattern) >>= \case
        Just []    -> return Nothing
        Just tags  -> filterTags tags
        Nothing    -> return Nothing
  where
    filterTags tags =
        maybeOneTag <$> filterM isCommitTag tags

    maybeOneTag :: [T.Text] -> Maybe T.Text
    maybeOneTag []    = Nothing
    maybeOneTag [tag] = Just tag
    maybeOneTag _     = Nothing

    -- | Return True if the tag is on the commit
    isCommitTag :: T.Text -> IO Bool
    isCommitTag tag = do
        -- Find the commit for this tag and check that it matches commit_id
        -- If so, return the branch/filename/r* part of the tag
        let ref_tag = T.pack $ printf "refs/tags/%s" tag
        ref <- Git.repositoryLookupReference repo ref_tag >>= maybeThrow LookupReferenceError
        tag_oid <- Git.refGetTarget ref >>= maybeThrow GetTargetError
        tag_obj <- Git.repositoryLookupTag repo tag_oid >>= maybeThrow LookupTagError
        oid <- Git.tagGetTargetId tag_obj >>= maybeThrow GetTargetIdError

        cmp <- Git.oIdCompare oid commit_id
        return $ cmp == 0

-- | Get the revision number from a git tag
--
-- [@mtag@]: The tag string to extract the revision from
--
-- The Tag is of the form refs/tags/\<branch\>/\<filename\>/r\<revision\>
--
-- Returns the revision from the tag, or Nothing
getRevisionFromTag :: Maybe T.Text -> Maybe Int
getRevisionFromTag mtag = case mtag of
    Nothing  -> Nothing
    Just tag -> getRevision $ T.unpack tag
  where
    getRevision :: String -> Maybe Int
    getRevision tag = do
        -- Get the digits after the final r
        let rs = elemIndices 'r' tag
        if null rs
            then Nothing
            else readMaybe $ drop (last rs + 1) tag

-- | Tag a recipe's most recent commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@recipe_name@]: Recipe name (not filename)
--
-- Returns True if it is successful
tagRecipeCommit :: Git.Repository -> T.Text -> T.Text -> IO Bool
tagRecipeCommit repo branch recipe_name = tagFileCommit repo branch (recipeTomlFilename $ T.unpack recipe_name)

-- | Tag a file's most recent commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: Recipe filename
--
-- This uses git tags, of the form refs/tags/\<branch\>/\<filename\>/r\<revision\>
-- Only the most recent recipe commit can be tagged to prevent out of order tagging.
-- Revisions start at 1 and increment for each new commit that is tagged.
-- If the commit has already been tagged it will return False.
--
-- Can throw 'NewSignatureError', 'NewOIdError', 'LookupError'
tagFileCommit :: Git.Repository -> T.Text -> T.Text -> IO Bool
tagFileCommit repo branch filename = do
    commits <- listCommits repo branch filename
    let rev_commit = findLastRev commits
    -- If there are no commits, or the most recent one has already been tagged, return False
    if null commits || isFirstCommit commits rev_commit
        then return False
        else tagNewestCommit (head commits) rev_commit
  where
    -- | Tag the most recent commit
    tagNewestCommit :: CommitDetails -> Maybe CommitDetails -> IO Bool
    tagNewestCommit last_commit rev_commit = do
        -- What revision is this? rev_commit may be Nothing, or cdRevision may be Nothing. Use 1 for those cases
        let rev = if isJust rev_commit && isJust (cdRevision (fromJust rev_commit))
                  then fromJust (cdRevision (fromJust rev_commit)) + 1
                  else 1

        let name = T.pack $ printf "%s/%s/r%d" branch filename rev
        sig <- Git.signatureNewNow "bdcs-api" "user-email" >>= maybeThrow NewSignatureError
        commit_id <- Git.oIdNewFromString (cdCommit last_commit) >>= maybeThrow NewOIdError
        commit_type <- gobjectType (undefined :: Git.Commit)
        commit_obj <- Git.repositoryLookup repo commit_id commit_type >>= maybeThrow LookupError
        mtag_id <- Git.repositoryCreateTag repo name commit_obj sig name [Git.CreateFlagsNone]
        return $ isJust mtag_id

    -- | Find the last revision in the commits and return it
    findLastRev :: [CommitDetails] -> Maybe CommitDetails
    findLastRev []= Nothing
    findLastRev (x:xs) = case cdRevision x of
                             Nothing  -> findLastRev xs
                             Just _   -> Just x

    -- | Is the revision commit the most recent one?
    --
    -- If it is, then we cannot make a new tag.
    -- If it is not, or there is no rev_commit, we can tag a new one.
    isFirstCommit :: [CommitDetails] -> Maybe CommitDetails -> Bool
    isFirstCommit _ Nothing           = False
    isFirstCommit [] _                = False
    isFirstCommit (c:_) (Just commit) = commit == c


-- | Commit a Recipe TOML file
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@filename@]: Recipe filename
--
-- Returns the OId of the new commit
commitRecipeFile :: Git.Repository -> T.Text -> FilePath -> IO Git.OId
commitRecipeFile repo branch filename = do
    toml_in <- TIO.readFile filename
    let erecipe = parseRecipe toml_in
    -- XXX Handle errors
    let recipe = head $ rights [erecipe]
    commitRecipe repo branch recipe

-- | Commit a Recipe record to a branch
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@recipe@]: Recipe record
--
-- If there is already an existing recipe this will bump or replace the
-- version number depending on what the new recipe contains. See the rules
-- in 'bumpVersion'
commitRecipe :: Git.Repository -> T.Text -> Recipe -> IO Git.OId
commitRecipe repo branch recipe = do
    old_version <- getOldVersion (T.pack $ rName recipe)
    -- Bump the recipe's version
    let erecipe = recipeBumpVersion recipe old_version
    -- XXX Handle errors
    let recipe' = head $ rights [erecipe]
    let version = fromJust (rVersion recipe')
    let toml_out = encodeUtf8 $ recipeTOML recipe'
    let filename = recipeTomlFilename (rName recipe')
    let message = T.pack $ printf "Recipe %s, version %s saved" filename version
    writeCommit repo branch filename message toml_out
  where
    getOldVersion :: T.Text -> IO (Maybe String)
    getOldVersion recipe_name = do
        eold_recipe <- readRecipeCommit repo branch recipe_name Nothing
        case eold_recipe of
            Left  _          -> return Nothing
            Right old_recipe -> return $ rVersion old_recipe

-- | Commit recipes from a directory, if they don't already exist
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@directory@]: Directory to read the recipes from
--
-- This reads all files ending in .toml from the directory, skipping recipes that
-- are already in the branch.
commitRecipeDirectory :: Git.Repository -> T.Text -> FilePath -> IO [Git.OId]
commitRecipeDirectory repo branch directory = do
    branch_files <- listBranchFiles repo branch
    files <- map (directory </>) . filter (skipFiles branch_files) <$> listDirectory directory
    mapM (commitRecipeFile repo branch) files
  where
    skipFiles :: [T.Text] -> String -> Bool
    skipFiles branch_files file = T.pack file `notElem` branch_files && ".toml" `isSuffixOf` file

-- | Read a Recipe from a commit
--
-- [@repo@]: Open git repository
-- [@branch@]: Branch name
-- [@recipe_name@]: Recipe name (not filename)
-- [@commit@]: The commit hash string to read
--
-- If the recipe isn't found it returns a Left
readRecipeCommit :: Git.Repository -> T.Text -> T.Text -> Maybe T.Text -> IO (Either String Recipe)
readRecipeCommit repo branch recipe_name commit = do
    -- Is this file in the branch?
    branch_files <- listBranchFiles repo branch
    let filename = recipeTomlFilename $ T.unpack recipe_name
    if filename `notElem` branch_files
        then return $ Left (printf "%s is not present on branch %s" filename branch)
        else do
            recipe_toml <- readCommit repo branch filename commit
            return $ parseRecipe (decodeUtf8 recipe_toml)

-- | print the OId
--
-- [@oid@]: The OId to print
--
-- Used for debugging
printOId :: Git.OId -> IO ()
printOId oid =
    Git.oIdToString oid >>= print


-- | Type of Diff Entry
--
-- Used by RecipeDiffEntry's old and new fields
data RecipeDiffType =
    Name {rdtName :: String}                                    -- ^ Name changed
  | Description {rdtDescription :: String}                      -- ^ Description changed
  | Version {rdtVersion :: Maybe String}                        -- ^ Version changed
  | Module {rdtModule :: RecipeModule}                          -- ^ Module version changed, added, or removed
  | Package {rdtPackage :: RecipeModule}                        -- ^ Package version changed, added, or removed
  | None                                                        -- ^ Used for added and removed
  deriving (Eq, Show)

instance ToJSON RecipeDiffType where
  toJSON Name{..}        = object ["Name" .= rdtName]
  toJSON Description{..} = object ["Description" .= rdtDescription]
  toJSON Version{..}     = object ["Version" .= rdtVersion]
  toJSON Module{..}      = object ["Module" .= toJSON rdtModule]
  toJSON Package{..}     = object ["Package" .= toJSON rdtPackage]
  toJSON None            = toJSON Null

instance FromJSON RecipeDiffType where
  parseJSON = withObject "Recipe diff type" $ \o -> asum [
    Name <$> o .: "Name",
    Description <$> o .: "Description",
    Version <$> o .: "Version",
    Module <$> parseJSON (Object o),
    Package <$> parseJSON (Object o) ]

-- | A difference entry
--
-- This uses RecipeDiffType to indicate the type of difference between
-- recipe fields.
--
-- If old is set and new is None it means the entry was removed
-- If old is None and new is set it means the entry was added
-- If both are set then old the the old content and new is the new content
data RecipeDiffEntry =
    RecipeDiffEntry {
        rdeOld :: RecipeDiffType,
        rdeNew :: RecipeDiffType
    } deriving (Eq, Show)

instance ToJSON RecipeDiffEntry where
  toJSON RecipeDiffEntry{..} = object [
      "old" .= rdeOld
    , "new" .= rdeNew ]

instance FromJSON RecipeDiffEntry where
  parseJSON = withObject "Recipe diff entry" $ \o -> do
    rdeOld <- o .: "old"
    rdeNew <- o .: "new"
    return RecipeDiffEntry{..}

-- | Find the differences between two recipes
--
-- [@oldRecipe@]: The old version of the Recipe
-- [@newRecipe@]: The new version of the Recipe
--
-- This calculates the differences between the recipes, returning a list of 'RecipeDiffEntry'.
-- The order is always the same, Name, Description, Version, removed Modules, added Modules,
-- removed Packages, added Packages, and then packages with different versions.
recipeDiff :: Recipe -> Recipe -> [RecipeDiffEntry]
recipeDiff oldRecipe newRecipe = do
    let removed_modules = removed_diff module_removed (rModules oldRecipe) (rModules newRecipe)
    let removed_packages = removed_diff package_removed (rPackages oldRecipe) (rPackages newRecipe)
    let added_modules = added_diff module_added (rModules oldRecipe) (rModules newRecipe)
    let added_packages = added_diff package_added (rPackages oldRecipe) (rPackages newRecipe)
    let same_modules  = same_diff module_diff (rModules oldRecipe) (rModules newRecipe)
    let same_packages = same_diff package_diff (rPackages oldRecipe) (rPackages newRecipe)
    let diffs = [name_diff oldRecipe newRecipe,
                 description_diff oldRecipe newRecipe,
                 version_diff oldRecipe newRecipe
                ] ++ removed_modules ++ added_modules ++ same_modules
                  ++ removed_packages ++ added_packages ++ same_packages

    map fromJust (filter isJust diffs)
  where
    -- | Return a list of the modules/packages that have been added
    --
    -- diff_f is a function that returns a RecipeDiffEntry (eg. module_added, package_added)
    -- o and m are lists of the old and new RecipeModules
    added_diff :: (RecipeModule -> Maybe RecipeDiffEntry) -> [RecipeModule] -> [RecipeModule] -> [Maybe RecipeDiffEntry]
    added_diff diff_f o n = map (diff_f . new_m) added_m
      where
        -- | Return a list of the added module names
        added_m :: [String]
        added_m = sortBy caseInsensitive $ toList $ module_names n `difference` module_names o
        -- | Lookup a recipe module name in the list of new modules
        new_m :: String -> RecipeModule
        new_m m = get_module m n

    -- | Return a list of the modules/packages that have been removed
    --
    -- diff_f is a function that returns a RecipeDiffEntry (eg. module_removed, package_removed)
    -- o and m are lists of the old and new RecipeModules
    removed_diff :: (RecipeModule -> Maybe RecipeDiffEntry) -> [RecipeModule] -> [RecipeModule] -> [Maybe RecipeDiffEntry]
    removed_diff diff_f o n = map (diff_f . old_m) removed_m
      where
        -- | Return a list of the removed module names
        removed_m :: [String]
        removed_m = sortBy caseInsensitive $ toList $ module_names o `difference` module_names n
        -- | Lookup a recipe module name in the list of old modules
        old_m :: String -> RecipeModule
        old_m m = get_module m o

    -- | Return a list of changes to modules/packages that are in both old and new lists
    --
    -- diff_f is a function that returns a RecipeDiffEntry (eg. module_diff, package_diff)
    -- o and m are lists of the old and new RecipeModules
    same_diff :: (RecipeModule -> RecipeModule -> Maybe RecipeDiffEntry) -> [RecipeModule] -> [RecipeModule] -> [Maybe RecipeDiffEntry]
    same_diff diff_f o n = map (\m -> diff_f (old_m m) (new_m m)) same_m
      where
        -- | Return a list of the module names that are in both lists
        same_m :: [String]
        same_m = sortBy caseInsensitive $ toList $ module_names o `intersection` module_names n
        -- | Lookup a recipe module name in the list of old modules
        old_m :: String -> RecipeModule
        old_m m = get_module m o
        -- | Lookup a recipe module name in the list of old modules
        new_m :: String -> RecipeModule
        new_m m = get_module m n

    -- | Check the recipe name for a change
    name_diff :: Recipe -> Recipe -> Maybe RecipeDiffEntry
    name_diff o n =
        if rName o == rName n then Nothing else
            Just $ RecipeDiffEntry (Name (rName o)) (Name (rName n))

    -- | Check the recipe description for a change
    description_diff :: Recipe -> Recipe -> Maybe RecipeDiffEntry
    description_diff o n =
        if rDescription o == rDescription n then Nothing else
            Just $ RecipeDiffEntry (Description (rDescription o)) (Description (rDescription n))

    -- | Check the recipe version for a change
    version_diff :: Recipe -> Recipe -> Maybe RecipeDiffEntry
    version_diff o n =
        if rVersion o == rVersion n then Nothing else
            Just $ RecipeDiffEntry (Version $ rVersion o) (Version $ rVersion n)

    -- | Check the module for a different version
    --
    -- Returns a Module RecipeDiffType with the module details
    module_diff :: RecipeModule -> RecipeModule -> Maybe RecipeDiffEntry
    module_diff o n =
        if rmVersion o == rmVersion n then Nothing else
            Just $ RecipeDiffEntry (Module o) (Module n)

    -- | Check the package for a different version
    --
    -- Returns a Package RecipeDiffType with the module details
    package_diff :: RecipeModule -> RecipeModule -> Maybe RecipeDiffEntry
    package_diff o n =
        if rmVersion o == rmVersion n then Nothing else
            Just $ RecipeDiffEntry (Package o) (Package n)

    -- | Return an entry with a removed module
    module_removed :: RecipeModule -> Maybe RecipeDiffEntry
    module_removed o = Just $ RecipeDiffEntry (Module o) None

    -- | Return an entry with a removed package
    package_removed :: RecipeModule -> Maybe RecipeDiffEntry
    package_removed o = Just $ RecipeDiffEntry (Package o) None

    -- | Return an entry with an added module
    module_added :: RecipeModule -> Maybe RecipeDiffEntry
    module_added n = Just $ RecipeDiffEntry None (Module n)

    -- | Return an entry with an added package
    package_added :: RecipeModule -> Maybe RecipeDiffEntry
    package_added n = Just $ RecipeDiffEntry None (Package n)

    -- ! Return a Set of the module/package names
    module_names :: [RecipeModule] -> Set String
    module_names modules = fromList $ map rmName modules

    -- | Get the recipe module from the list
    --
    -- Only call this with module names that are known to be in the list
    get_module :: String -> [RecipeModule] -> RecipeModule
    get_module module_name module_list = fromJust $ find (\e -> rmName e == module_name) module_list


-- =========================
-- Test Functions Below Here
--
-- These functions exist here because there is no way (that I know of) to setup a framework in
-- Spec and run a series of tests in order that depend on a temporary Git repository.
testRecipe :: Recipe
testRecipe =
    Recipe {rName = "test-server",
            rVersion = Just "0.1.2",
            rDescription = "Testing git commit of a Recipe record",
            rPackages = [RecipeModule {rmName = "tmux", rmVersion = "2.2"},
                         RecipeModule {rmName = "openssh-server", rmVersion = "6.6.*"},
                         RecipeModule {rmName = "rsync", rmVersion = "3.0.*"}],
            rModules = [RecipeModule {rmName = "httpd", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.4"},
                        RecipeModule {rmName = "mod_ssl", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "php", rmVersion = "5.4.*"},
                        RecipeModule {rmName = "php-mysql", rmVersion = "5.4.*"}]
    }

testFiles :: [T.Text]
testFiles  = ["glusterfs.toml","http-server.toml","kubernetes.toml","test-fake.toml","test-server.toml"]
testFiles2 :: [T.Text]
testFiles2 = ["glusterfs.toml","kubernetes.toml","test-fake.toml","test-server.toml"]

data TestError =
    FileListError [T.Text]
  | ListCommitsError
  | HttpCommitError [CommitDetails]
  | TagCommitError
  | CommitRevisionError [CommitDetails]
  | DeleteFailedError FilePath
  | RecipeReadError
  | RecipeMismatchError [Recipe]
  | ChangesOrderError
  deriving (Eq, Show)

instance Exception TestError

-- | Run the Git repository tests with a temporary directory
runGitRepoTests :: IO Bool
runGitRepoTests = withTempDirectory "/var/tmp/" "bdcsgit-test" testGitRepo

-- | Test the Git repository functions
testGitRepo :: FilePath -> IO Bool
testGitRepo tmpdir = do
    Git.init
    repo <- openOrCreateRepo tmpdir

    -- Commit a file to the repo
    putStrLn "    - Committing http-server.toml"
    void $ commitRecipeFile repo "master" "./tests/recipes/http-server.toml"

    -- Commit a directory to the repo
    putStrLn "    - Committing a directory of recipes"
    void $ commitRecipeDirectory repo "master" "./tests/recipes/"

    -- Commit a Recipe record to the repo
    putStrLn "    - Committing a Recipe record"
    void $ commitRecipe repo "master" testRecipe

    -- Check that the testRecipe's version was not bumped on 1st save
    putStrLn "    - Checking Recipe Version"
    erecipe <- readRecipeCommit repo "master" "test-server" Nothing
    let recipe = head $ rights [erecipe]
    unless (testRecipe == recipe) (throwIO $ RecipeMismatchError [testRecipe, recipe])

    -- Check that saving a changed recipe, with the same version, bumps it.
    let new_recipe1      = testRecipe { rDescription = "Second commit with same version, should bump" }
    putStrLn "    - Committing a Recipe record with changed description"
    void $ commitRecipe repo "master" new_recipe1

    -- Check that the version was bumped on the 2nd save
    putStrLn "    - Checking Modified Recipe's Version"
    erecipe' <- readRecipeCommit repo "master" "test-server" Nothing
    let recipe' = head $ rights [erecipe']
    unless (new_recipe1 {rVersion = Just "0.1.3"} == recipe') (throwIO $ RecipeMismatchError [new_recipe1, recipe'])

    -- Check that saving a changed recipe, with a completely different version, uses it without bumping.
    let new_recipe2 = testRecipe {rDescription = "Third commit with new version, should just use it",
                                  rVersion = Just "0.3.1"}
    putStrLn "    - Committing a Recipe record with changed description and different version"
    void $ commitRecipe repo "master" new_recipe2

    -- Check that the version was used as-is
    putStrLn "    - Checking Modified Recipe's Version"
    erecipe'' <- readRecipeCommit repo "master" "test-server" Nothing
    let recipe'' = head $ rights [erecipe'']
    unless (new_recipe2 == recipe'') (throwIO $ RecipeMismatchError [new_recipe2, recipe''])

    -- List the files on master
    putStrLn "    - Listing the committed files"
    files <- listBranchFiles repo "master"
    unless (files == testFiles) (throwIO $ FileListError files)

    -- Get the commits to http-server.toml
    putStrLn "    - List commits to http-server.toml"
    http_commits <- listCommits repo "master" "http-server.toml"
    -- Should be 1 commit
    let expected_msg_1 = "Recipe http-server.toml, version 0.2.0 saved"
    let msg_1 = cdMessage (head http_commits)
    unless (msg_1 == expected_msg_1) (throwIO $ HttpCommitError http_commits)

    -- delete http-server.toml file
    putStrLn "    - Delete the http-server.toml file"
    void $ deleteRecipe repo "master" "http-server"

    -- List the files on master
    putStrLn "    - Check that http-server.toml has been deleted"
    files2 <- listBranchFiles repo "master"
    unless (files2 == testFiles2) (throwIO $ FileListError files2)

    -- Revert the delete
    commit_id <- Git.oIdNewFromString (cdCommit $ head http_commits) >>= maybeThrow NewOIdError
    revert_id <- revertFileCommit repo "master" "http-server.toml" commit_id

    -- List the files on master
    putStrLn "    - Check that http-server.toml has been restored"
    files3 <- listBranchFiles repo "master"
    unless (files3 == testFiles) (throwIO $ FileListError files3)

    -- tag a commit
    putStrLn "    - Tag most recent commit of http-server.toml"
    ok <- tagRecipeCommit repo "master" "http-server"
    unless ok (throwIO TagCommitError)

    -- list the commits and check for the tag
    putStrLn "    - Check the Tag"
    commits <- listCommits repo "master" "http-server.toml"
    let revision = cdRevision (head commits)
    unless (revision == Just 1) (throwIO $ CommitRevisionError commits)

    -- Make sure the first listed commit is the reverted commit
    let top_commit = cdCommit $ head commits
    revert_hash <- fromJust <$> Git.oIdToString revert_id
    unless (top_commit == revert_hash) (throwIO $ ChangesOrderError)

    return True


-- | Run the Workspace tests with a temporary directory
runWorkspaceTests :: IO Bool
runWorkspaceTests = withTempDirectory "/var/tmp/" "bdcsws-test" testWorkspace

-- | Test the Workspace functions
testWorkspace :: FilePath -> IO Bool
testWorkspace tmpdir = do
    Git.init
    repo <- openOrCreateRepo tmpdir

    -- Write the Recipe to workspace for master branch
    putStrLn "    - Write testRecipe to Workspace for master branch"
    workspaceWrite repo "master" testRecipe

    -- Read the Recipe, does it match?
    putStrLn "    - Read Recipe from Workspace for master branch"
    recipe <- workspaceRead repo "master" "test-server" >>= maybeThrow RecipeReadError
    unless (testRecipe == recipe) (throwIO $ RecipeMismatchError [testRecipe, recipe])

    -- Delete the Recipe, is it gone?
    putStrLn "    - Delete Recipe from Workspace for master branch"
    workspaceDelete repo "master" "test-server"
    dir <- workspaceDir repo "master"
    let filename = dir </> T.unpack (recipeTomlFilename $ T.unpack "test-server")
    whenM (doesFileExist filename) (throwIO $ DeleteFailedError filename)

    return True
