{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import System.Exit (exitSuccess)

import Dots

-- | Accelerate player's 'Dot'.
-- This only sends a command to server.
accel :: Dir -> GameState -> IO GameState
accel d g@GameState{..} = do
  -- we fork to avoid interface freezing
  _ <- forkIO $ sendBinaryData gameConnection (ActionAccel d)
  return g

-- | Handle user input.
handleGame :: Event -> GameState -> IO GameState
handleGame (EventKey (SpecialKey KeyEsc)   Down _ _) = const exitSuccess    -- exit on ESC
handleGame (EventKey (SpecialKey KeyLeft)  Down _ _) = accel L
handleGame (EventKey (SpecialKey KeyRight) Down _ _) = accel R
handleGame (EventKey (SpecialKey KeyUp)    Down _ _) = accel U
handleGame (EventKey (SpecialKey KeyDown)  Down _ _) = accel D
handleGame _ = return

-- | Handle 'Universe' updates coming from server.
handleUpdates :: GameState -> IO ()
handleUpdates GameState{..} = forever $ do
  universe <- receiveData gameConnection
  atomically $ writeTVar gameUniverse universe

-- | Draw the current state of the 'Universe'.
drawGame :: GameState -> IO Picture
drawGame GameState{..} = drawUniverse <$> readTVarIO gameUniverse

-- | This does nothing since updates come from server.
-- See 'handleUpdates'.
updateGame :: Float -> GameState -> IO GameState
updateGame _dt gs = return gs

-- | Game state on client.
data GameState = GameState
  { gameUniverse    :: TVar Universe    -- ^ The current state of the universe.
  , gameConnection  :: Connection       -- ^ Websocket 'Connection' with the server.
  }

main :: IO ()
main = do
  universe <- atomically $ newTVar emptyUniverse
  runClient "localhost" 8000 "/connect" $ \conn -> do
    let gs = GameState universe conn
    _ <- forkIO (handleUpdates gs)
    playIO display bgColor fps (GameState universe conn) drawGame handleGame updateGame
  where
    winSize   = (500, 500)
    winOffset = (100, 100)
    display   = InWindow "Dots" winSize winOffset
    bgColor   = black
    fps       = 30
