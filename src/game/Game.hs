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
import Graphics
import Configs

import Data.Word ( Word32 )

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
    mGameState <- runAppEnv appEnvData updateGameState
    let stop = inputQuit input
    case (mGameState, stop) of
        (Nothing, _) -> cleanup
        (_, True)    -> cleanup
        (Just gs, _) -> do
            cfgs <- runAppEnv appEnvData readConfigs
            let gr' = updateGraphics (appEnvDataGraphics appEnvData) cfgs input
                appEnvData' = appEnvData
                    { appEnvDataInputState = input
                    , appEnvDataGameState  = gs
                    , appEnvDataGraphics   = gr'
                    }
            runGame appEnvData'
  where
    cleanup = do
        outputs <- runAppEnv appEnvData getOutputs
        cleanupOutputHandles outputs

stepGame :: AppEnv InputState
stepGame = do
    draws <- updateWindow
    _ <- executeDraw draws
    fr <- readFrameRate
    updateInput $ frameTime fr
