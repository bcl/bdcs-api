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
import           BDCS.API.Config(AskTuple, ServerConfig(..))
import           BDCS.API.Recipes(openOrCreateRepo, commitRecipeDirectory)
import           BDCS.API.Utils(GitLock(..))
import           BDCS.API.V0(V0API, v0ApiServer)
import           Control.Concurrent(ThreadId, forkFinally, forkIO)
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Concurrent.STM.TChan(newTChan, tryReadTChan)
import           Control.Concurrent.STM.TMVar(putTMVar)
import           Control.Conditional(whenM)
import           Control.Monad(forever, void)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(runStderrLoggingT)
import           Control.Monad.STM(atomically)
import           Data.Aeson
import           Data.IORef(IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import           Data.Maybe(isNothing)
import           Data.String.Conversions(cs)
import           Database.Persist.Sqlite
import qualified GI.Ggit as Git
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options
import           Servant
import           System.Directory(createDirectoryIfMissing)

-- | The status of the server, the database, and the API.
data ServerStatus = ServerStatus
  {  srvVersion   :: String                                     -- ^ Server version
  ,  srvSchema    :: String                                     -- ^ Supported Database Schema version
  ,  srvDb        :: String                                     -- ^ Database version
  ,  srvSupported :: Bool                                       -- ^ True if the Database is supported by the Server
  } deriving (Eq, Show)

instance ToJSON ServerStatus where
  toJSON ServerStatus{..} = object [
      "version"   .= srvVersion
    , "schema"    .= srvSchema
    , "db"        .= srvDb
    , "supported" .= srvSupported ]

instance FromJSON ServerStatus where
  parseJSON = withObject "server status" $ \o -> do
    srvVersion   <- o .: "version"
    srvSchema    <- o .: "schema"
    srvDb        <- o .: "db"
    srvSupported <- o .: "supported"
    return ServerStatus{..}

-- | The /status route
type CommonAPI = "status" :> Get '[JSON] ServerStatus

serverStatus :: Handler ServerStatus
serverStatus = return (ServerStatus "0.0.0" "0" "0" False)

commonServer :: Server CommonAPI
commonServer = serverStatus

-- | The combined API routes, /status and /api/v0/*
type CombinedAPI = CommonAPI
              :<|> "api" :> "v0" :> V0API

combinedServer :: ServerConfig -> Server CombinedAPI
combinedServer cfg = commonServer
                :<|> v0ApiServer cfg

-- | CORS policy
appCors :: Middleware
appCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
             { corsRequestHeaders = ["Content-Type"]
             , corsMethods = "PUT" : simpleMethods }

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

    q    <- newIORef []
    chan <- atomically newTChan

    let cfg = ServerConfig { cfgRepoLock = GitLock lock repo,
                             cfgChan = chan,
                             cfgWorkQ = q,
                             cfgPool = pool,
                             cfgBdcs = bdcsPath,
                             cfgResultsDir = "/var/tmp/composer" }

    createDirectoryIfMissing True (cfgResultsDir cfg)

    -- Fork off another process that does the composes in the background,
    -- which means the client immediately gets a response with a build ID.
    -- The compose (which could take a while) proceeds independently.  The
    -- client uses a different route to check and fetch the results.
    void $ forkIO $ composeServer cfg

    return $ app cfg

-- | Run the API server
runServer :: Int -> FilePath -> FilePath -> FilePath -> IO ()
runServer port bdcsPath gitRepoPath sqliteDbPath = run port =<< mkApp bdcsPath gitRepoPath sqliteDbPath

composeServer :: ServerConfig -> IO ()
composeServer ServerConfig{..} = do
    -- A mutable variable to hold information about the currently running compose.
    -- Since we just support one compose at a time, this will just store a tuple of
    -- thread ID and ComposeInfo record.  If this is Nothing, no compose is currently
    -- running.
    cur <- newIORef Nothing

    -- We run in a loop forever doing two tasks:
    --
    -- (1) Reading questions out of the shared one-way communications channel and
    -- sending responses back to the API server.
    --
    -- (2) Firing off composes for requests in the queue.
    forever $ do
        atomically (tryReadTChan cfgChan) >>= respondToMessage cur

        -- If the mutable variable is not Nothing, we are already running a compose.
        -- Don't try to start another one.  This leaves the work queue alone so we
        -- can grab the next item later.
        whenM (isNothing <$> readIORef cur) $
            nextInQ cfgWorkQ >>= \case
                -- Start another compose.  When the thread finishes (either because the
                -- compose is done or because it failed), clear out the mutable variable.
                Just ci -> do threadId <- forkFinally (compose cfgBdcs cfgPool ci)
                                                      (\_ -> atomicWriteIORef cur Nothing)

                              atomicWriteIORef cur (Just (threadId, ci))

                _ -> return ()
 where
    -- Pop the next element off the head of the queue and return it.
    nextInQ :: IORef [a] -> IO (Maybe a)
    nextInQ q = atomicModifyIORef' q (\case (hd:tl) -> (tl, Just hd)
                                            _       -> ([], Nothing))

    respondToMessage :: IORef (Maybe (ThreadId, ComposeInfo)) -> Maybe AskTuple -> IO ()
    respondToMessage _ (Just (AskBuildsWaiting, r)) = do
        q <- readIORef cfgWorkQ
        atomically $ putTMVar r (RespBuildsWaiting $ map ciId q)

    respondToMessage cur (Just (AskBuildsInProgress, r)) =
        readIORef cur >>= \case
            Nothing                   -> atomically $ putTMVar r (RespBuildsInProgress [])
            Just (_, ComposeInfo{..}) -> atomically $ putTMVar r (RespBuildsInProgress [ciId])

    respondToMessage _ _ = return ()
