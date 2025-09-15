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

framesPerSecond :: Word32
framesPerSecond = 45

-- Time for a frame
frameTime :: Word32
frameTime = div 1000000000 (framesPerSecond + 1)

-- Game loop that enforces a frame rate throttling
runGame :: Word32 -> SystemTime -> AppEnvData -> IO ()
runGame count pTime appEnvData = do
    time <- getSystemTime
    if systemSeconds time /= systemSeconds pTime
        then do
            -- new second, force a new frame so we just have to check
            -- nano second part of time difference
            run 0 time appEnvData
        else do
            let diff = ((systemNanoseconds time) - (systemNanoseconds pTime))
            -- only "run" at the frame rate
            -- this means the "frame rate" limiter limits the speed of the whole game
            -- TODO: maybe in future push this into "output handles" so can still read inputs
            -- in "real" time
            if diff < frameTime
                then do
                    -- wait another loop cycle before running game
                    -- TODO: a sleep for frameTime - diff?
                    threadDelay (fromIntegral (diff `div` 1000)) -- number of microseconds
                    runGame count pTime appEnvData
                else
                    -- step the state of the game
                    run (count + 1) time appEnvData

-- Run the game for one loop iteration
--      - "step" the game state with the old inputs
--      -
run :: Word32 -> SystemTime -> AppEnvData -> IO ()
run count time appEnvData = do
    inputs <- runAppEnv appEnvData stepGame
    applyInputs count time inputs appEnvData


applyInputs :: Word32 -> SystemTime -> [InputState] -> AppEnvData -> IO ()
applyInputs _ _ [] _ = return ()
applyInputs count time (input:tl) appEnvData = do
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
        _ -> runGame count time appEnvData'

    applyInputs count time tl appEnvData'


stepGame :: AppEnv [InputState]
stepGame = do
    draws <- updateWindow
    _ <- executeDraw draws
    updateInput
