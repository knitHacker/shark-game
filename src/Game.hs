{-# LANGUAGE Strict #-}
module Game
    ( runGame
    ) where

import OutputHandles.Types ( OutputRead(getOutputs) )
import OutputHandles ( cleanupOutputHandles, executeDraw )
import Env ( runAppEnv )
import Env.Types
    ( AppEnv, AppEnvData(appEnvDataInputState, appEnvDataGameState) )
import InputState
import GameState ( isGameExiting, updateGameState )
import GameState.Draw ( updateWindow )
import GameState.Types ( GameState(..), GameView(..) )
import SaveData
import Configs ( ConfigsRead(..), GameConfigs(..), StateConfigs(..), updateStateConfigs )

import qualified SDL
import Control.Monad.IO.Class ()
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Time.Clock.System
    ( SystemTime(systemSeconds, systemNanoseconds), getSystemTime )
import Data.Word ( Word32 )
import Control.Concurrent
import Control.Arrow (ArrowApply(app))

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

    case (isGameExiting gameState', stop) of
        (True, _) -> do
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
    updateInput $ traceShowId $ frameTime fr
