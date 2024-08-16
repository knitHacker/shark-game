{-# LANGUAGE Strict #-}
module Game
    ( runGame
    ) where

import OutputHandles.Types ( OutputRead(getOutputs) )
import OutputHandles ( cleanupOutputHandles, executeDraw )
import Env ( runAppEnv )
import Env.Types
    ( AppEnv, AppEnvData(appEnvDataInputState, appEnvDataGameState) )
import InputState ( updateInput, InputState(inputStateQuit) )
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

framesPerSecond :: Word32
framesPerSecond = 60

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
    input <- runAppEnv appEnvData stepGame
    gameState' <- runAppEnv appEnvData updateGameState
    let stop = inputStateQuit input
        appEnvData' = appEnvData { appEnvDataInputState = input, appEnvDataGameState = gameState' }

    case (gameView gameState', stop) of
        (GameExiting Nothing, _) -> do
            outputs <- runAppEnv appEnvData getOutputs
            cleanupOutputHandles outputs
        (GameExiting (Just gd), _) -> do
            outputs <- runAppEnv appEnvData getOutputs
            saveToFile gd
            cfgs <- runAppEnv appEnvData readConfigs
            let sc = stateCfgs cfgs
                sc' = sc { lastSaveM = Just (gameDataSaveFile gd) }
            updateStateConfigs sc'
            cleanupOutputHandles outputs
        (_, True) -> do
            outputs <- runAppEnv appEnvData getOutputs
            cleanupOutputHandles outputs
        _ -> runGame count time appEnvData'


stepGame :: AppEnv InputState
stepGame = do
    drawsM <- updateWindow
    case drawsM of
        Just draws -> executeDraw draws
        _ -> return ()
    updateInput
