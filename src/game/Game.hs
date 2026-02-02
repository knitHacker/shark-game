{-# LANGUAGE Strict #-}
module Game
    ( runGame
    ) where

import OutputHandles.Types
import OutputHandles
import Env
import Env.Types
import InputState
import GameState
import GameState.Draw
import GameState.Types
import SaveData
import Configs

import qualified SDL
import Control.Monad.IO.Class ()
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Time.Clock.System
    ( SystemTime(systemSeconds, systemNanoseconds)
    , getSystemTime
    )
import Data.Word ( Word32 )
import Control.Concurrent
import Control.Arrow

import Debug.Trace

-- Time for a frame
frameTime :: Word32 -> Word32
frameTime fps = div 1000 (fps + 1)

-- Game loop that enforces a frame rate throttling
runGame :: AppEnvData -> IO ()
runGame appEnvData = do
    input <- runAppEnv appEnvData stepGame
    applyInputs input appEnvData


applyInputs :: InputState -> AppEnvData -> IO ()
applyInputs input appEnvData = do
    gameState' <- runAppEnv appEnvData updateGameState
    let stop = inputQuit input
        appEnvData' = appEnvData { appEnvDataInputState = input, appEnvDataGameState = gameState' }

    case (gameView gameState', stop) of
        (GameExiting, _) -> do
            outputs <- runAppEnv appEnvData getOutputs
            cleanupOutputHandles outputs
        (_, True) -> do
            outputs <- runAppEnv appEnvData getOutputs
            cleanupOutputHandles outputs
        _ -> do
            runGame appEnvData'

stepGame :: AppEnv InputState
stepGame = do
    draws <- updateWindow
    _ <- executeDraw draws
    fr <- readFrameRate
    updateInput $ frameTime fr
