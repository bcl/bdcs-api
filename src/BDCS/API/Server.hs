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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-| BDCS API Server

    This starts a server and answers the API requests.
-}
module BDCS.API.Server(mkApp,
                       proxyAPI,
                       runServer,
                       ServerStatus(..))
  where

import           BDCS.API.Compose(ComposeInfo(..), ComposeMsgAsk(..), ComposeMsgResp(..), compose)
import           BDCS.API.Config(ServerConfig(..))
import           BDCS.API.Recipes(openOrCreateRepo, commitRecipeDirectory)
import           BDCS.API.Utils(GitLock(..))
import           BDCS.API.V0(V0API, v0ApiServer)
import           BDCS.API.Version(apiVersion)
import           BDCS.DB(schemaVersion, getDbVersion)
import           Control.Concurrent.Async(Async, async, cancel, concurrently_, waitCatch)
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Concurrent.STM.TChan(newTChan, readTChan)
import           Control.Concurrent.STM.TVar(TVar, modifyTVar, newTVar, readTVar, writeTVar)
import           Control.Concurrent.STM.TMVar(putTMVar)
import           Control.Monad(forever, void, when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.Logger(runFileLoggingT, runStderrLoggingT)
import           Control.Monad.STM(atomically)
import           Data.Aeson
import           Data.Int(Int64)
import           Data.IORef(IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import           Data.Sequence((|>), Seq(..), deleteAt, empty, findIndexL, index)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           GHC.Conc(retry)
import           GHC.Exts(toList)
import qualified GI.Ggit as Git
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           System.Directory(createDirectoryIfMissing, removePathForcibly)
import           System.FilePath.Posix((</>))

type InProgressMap = Map.Map T.Text (Async (), ComposeInfo)

-- | The status of the server, the database, and the API.
data ServerStatus = ServerStatus
  {  srvBackend   :: String                                     -- ^ Backend implementation (weldr, lorax-composer)
  ,  srvVersion   :: String                                     -- ^ Server version
  ,  srvSchema    :: Int64                                      -- ^ Supported Database Schema version
  ,  srvDb        :: Int64                                      -- ^ Database version
  ,  srvSupported :: Bool                                       -- ^ True if the Database is supported by the Server
  } deriving (Eq, Show)

instance ToJSON ServerStatus where
  toJSON ServerStatus{..} = object
    [ "backend"   .= srvBackend
    , "version"   .= srvVersion
    , "schema"    .= srvSchema
    , "db"        .= srvDb
    , "supported" .= srvSupported ]

instance FromJSON ServerStatus where
  parseJSON = withObject "server status" $ \o -> do
    srvBackend   <- o .: "backend"
    srvVersion   <- o .: "version"
    srvSchema    <- o .: "schema"
    srvDb        <- o .: "db"
    srvSupported <- o .: "supported"
    return ServerStatus{..}

-- | The /status route
type CommonAPI = "api" :> "status" :> Get '[JSON] ServerStatus


serverStatus :: ServerConfig -> Handler ServerStatus
serverStatus ServerConfig{..} = do
    version <- dbVersion
    return (ServerStatus "weldr" apiVersion schemaVersion version (schemaVersion == version))
  where
    dbVersion = do
        result <- runExceptT $ runSqlPool getDbVersion cfgPool
        case result of
            Left _        -> return 0
            Right version -> return version

commonServer :: ServerConfig -> Server CommonAPI
commonServer cfg = serverStatus cfg

-- | The combined API routes, /status and /api/v0/*
type CombinedAPI = CommonAPI
              :<|> "api" :> "v0" :> V0API

combinedServer :: ServerConfig -> Server CombinedAPI
combinedServer cfg = commonServer cfg
                :<|> v0ApiServer cfg

-- | CORS policy
appCors :: Middleware
appCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
             { corsRequestHeaders = ["Content-Type"]
             , corsMethods = "DELETE" : "PUT" : simpleMethods }

-- | Servant 'Proxy'
--
-- This connects the API to everything else
proxyAPI :: Proxy CombinedAPI
proxyAPI = Proxy

app :: ServerConfig -> Application
app cfg = appCors
        $ serve proxyAPI
        $ combinedServer cfg

-- | Create the server app
--
-- Create a SQLite connection pool, open/create the Git repo, and return the app
mkApp :: FilePath -> FilePath -> FilePath -> IO Application
mkApp bdcsPath gitRepoPath sqliteDbPath = do
    pool <- runStderrLoggingT $ createSqlitePool (cs sqliteDbPath) 5
--    runSqlPool (runMigration migrateAll) pool

    Git.init
    repo <- openOrCreateRepo gitRepoPath
    void $ commitRecipeDirectory repo "master" gitRepoPath
    lock <- RWL.new

    chan <- atomically newTChan

    let cfg = ServerConfig { cfgRepoLock = GitLock lock repo,
                             cfgChan = chan,
                             cfgPool = pool,
                             cfgBdcs = bdcsPath,
                             cfgResultsDir = "/var/lib/composer" }

    createDirectoryIfMissing True (cfgResultsDir cfg)

    -- Fork off another process that does the composes in the background,
    -- which means the client immediately gets a response with a build ID.
    -- The compose (which could take a while) proceeds independently.  The
    -- client uses a different route to check and fetch the results.
    void $ async $ composeServer cfg

    return $ app cfg

-- | Run the API server
runServer :: Int -> FilePath -> FilePath -> FilePath -> IO ()
runServer port bdcsPath gitRepoPath sqliteDbPath = run port =<< mkApp bdcsPath gitRepoPath sqliteDbPath

composeServer :: ServerConfig -> IO ()
composeServer ServerConfig{..} = do
    -- A mutable variable that lets us keep track about currently running composes.
    -- This is a map from UUID of the compose underway to the ThreadId doing that
    -- compose.  This lets us kill threads if needed.  If this is empty, no compose
    -- is currently running.
    inProgressRef <- newIORef Map.empty

    -- A list of all composes currently waiting to be run.  It would be easier if we
    -- could use a TChan to represent it, but there's no good way to grab the entire
    -- contents of one - it's a FIFO.  Thus we need to use some sort of list.
    worklist <- atomically $ newTVar empty

    -- From here, we run two separate threads forever.
    --
    -- One thread reads messages out of the channel and responds to them.  This includes
    -- things like "what is waiting in the queue?" and "what is currently composing?".
    -- It also includes requests to start new composes.
    --
    -- The other thread runs composes.  It does one at a time - reading the first item
    -- out of a worklist, starting the compose, and waiting for it to finish.  When one
    -- compose is finished, it can look at the list to see about starting the next one.
    concurrently_ (messagesThread inProgressRef worklist)
                  (composesThread inProgressRef worklist)
 where
    -- Add a newly started compose to the in progress map.
    addCompose :: IORef InProgressMap -> ComposeInfo -> Async () -> IO ()
    addCompose ref ci@ComposeInfo{..} thread =
        void $ atomicModifyIORef' ref (\m -> (Map.insert ciId (thread, ci) m, ()))

    -- Remove a completed (or killed?) compose from the in progress map.
    removeCompose :: IORef InProgressMap -> T.Text -> IO ()
    removeCompose ref uuid =
        void $ atomicModifyIORef' ref (\m -> (Map.delete uuid m, ()))

    composesThread :: IORef InProgressMap -> TVar (Seq ComposeInfo) -> IO ()
    composesThread inProgressRef worklist = forever $ do
        -- For now, we only support running one compose at a time.  If the mutable
        -- variable is not empty, we are already running a compose.  Don't try to
        -- start another one.  This leaves the work queue alone so we can grab the next
        -- item later.
        inProgress <- readIORef inProgressRef
        when (Map.null inProgress) $ do
            -- Start another compose and wait for it to be done.  When the thread
            -- finishes (either because the compose is done or because it failed),
            -- clear out the mutable variable.  Here, we don't actually care about
            -- whether it failed or finished - that's all handled elsewhere.
            ci <- atomically $ do
                lst <- readTVar worklist
                case lst of
                    -- This call to retry is very important.  Without it, any polling
                    -- on data structures like a TVar or an IORef keeps the CPU pegged
                    -- at 100% the entire time.
                    (x :<| xs) -> writeTVar worklist xs >> return x
                    _          -> retry

            thread <- async $ runFileLoggingT (ciResultsDir ci </> "compose.log")
                                              (compose cfgBdcs cfgPool ci)

            addCompose inProgressRef ci thread
            void $ waitCatch thread
            removeCompose inProgressRef (ciId ci)

    messagesThread :: IORef InProgressMap -> TVar (Seq ComposeInfo) -> IO ()
    messagesThread inProgressRef worklist = forever $ atomically (readTChan cfgChan) >>= \case
        (AskBuildsWaiting, Just r) -> do
            lst <- atomically $ readTVar worklist
            atomically $ putTMVar r (RespBuildsWaiting $ map ciId (toList lst))

        (AskBuildsInProgress, Just r) -> do
            -- Get just the ComposeInfo records for all the in-progress composes.
            inProgress <- map snd . Map.elems <$> readIORef inProgressRef
            -- And then extract the UUIDs of each, and that's the answer.
            atomically $ putTMVar r (RespBuildsInProgress $ map ciId inProgress)

        (AskCancelBuild buildId, Just r) -> do
            inProgress <- readIORef inProgressRef
            case Map.lookup buildId inProgress of
                Just (thread, ci) -> do cancel thread
                                        removeCompose inProgressRef buildId
                                        removePathForcibly (ciResultsDir ci)
                                        atomically $ putTMVar r (RespBuildCancelled True)

                _                 -> atomically $ putTMVar r (RespBuildCancelled False)

        (AskCompose ci, _) ->
            -- Add the new compose to the end of the work queue.  It will eventually
            -- get around to being run by composesThread.
            atomically $ modifyTVar worklist (|> ci)

        (AskDequeueBuild buildId, Just r) -> do
            -- The worklist stores ComposeInfo records, but we only get the UUID from the
            -- client.  So first we have to find the right element in the worklist.  Some
            -- element with that UUID should be present, but we can't guarantee that given
            -- all the multiprocessing stuff.  Hence the Maybe.
            ci <- atomically $ do
                lst <- readTVar worklist
                case findIndexL (\e -> ciId e == buildId) lst of
                    Nothing  -> return Nothing
                    Just ndx -> do let ele = index lst ndx
                                   modifyTVar worklist (deleteAt ndx)
                                   return $ Just ele

            -- If we found a ComposeInfo, clean it up - remove the results directory
            -- (that doesn't yet have an artifact, but should have some toml files) and
            -- inform the client.  We already removed it from the worklist in the block
            -- above.
            case ci of
                Just ComposeInfo{..} -> do
                    removePathForcibly ciResultsDir
                    atomically $ putTMVar r (RespBuildDequeued True)

                Nothing -> atomically $ putTMVar r (RespBuildDequeued False)

        _ -> return ()
