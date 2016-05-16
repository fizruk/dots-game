{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Dots.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant

import Dots

-- | Server config.
data Config = Config
  { configUniverse  :: TVar Universe            -- ^ The current state of the universe.
  , configClients   :: TVar (Map Name Client)   -- ^ All connected clients by a unique name.
  , configNames     :: TVar [Name]              -- ^ A list of default names (new client picks one from the list).
  , configColors    :: TVar [DotColor]          -- ^ A list of default colors (new client picks one from the list).
  }

-- | Default server config with empty universe and no clients.
-- Default names are of the form \"Player n\".
mkDefaultConfig :: IO Config
mkDefaultConfig = atomically $ Config
  <$> newTVar emptyUniverse
  <*> newTVar Map.empty
  <*> newTVar (map (\n -> "Player " ++ show n) [1..])
  <*> newTVar defaultDotColors

-- | A client is represented by its websocket 'Connection'.
type Client = Connection

-- | An API for the Dots Game server.
type DotsAPI = "connect" :> Raw

-- | The Dots Game server 'Application'.
server :: Config -> Server DotsAPI
server config = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        name <- addClient conn config
        putStrLn $ name ++ " joined!"
        handleActions name conn config

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Add a new client to the server state.
-- This will update 'configClients', take one element
-- from 'configNames' and 'configColors' and also add
-- a new player to the 'configUniverse'.
addClient :: Client -> Config -> IO Name
addClient client Config{..} = atomically $ do
  clients       <- readTVar configClients
  name:names    <- readTVar configNames
  color:colors  <- readTVar configColors
  writeTVar configClients (Map.insert name client clients)
  writeTVar configNames names
  writeTVar configColors colors
  modifyTVar configUniverse (addPlayer name color)
  return name

-- | An infinite loop, receiving data from the 'Client'
-- and handling its actions via 'handlePlayerAction'.
handleActions :: Name -> Connection -> Config -> IO ()
handleActions name conn Config{..} = forever $ do
  action <- receiveData conn
  atomically $ do
    modifyTVar configUniverse (handlePlayerAction name action)

-- | Periodically update the 'Universe' and send updates to all the clients.
periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  (universe, clients) <- atomically $ do
    universe <- readTVar configUniverse
    -- FIXME: (ms / 10^6) is not the actual time that has passed since the previous update
    -- we should use getCurrentTime to more accurately keep track of time deltas
    let universe' = updateUniverse (fromIntegral ms / 1000000) universe
    writeTVar configUniverse universe'
    clients <- readTVar configClients
    return (universe', clients)
  -- send every client updated universe
  -- we fork for every message because we want this to perform in parallel
  -- moreover if some clients fail we don't want exceptions to affect other clients
  mapM_ (\conn -> forkIO $ sendBinaryData conn universe) clients

