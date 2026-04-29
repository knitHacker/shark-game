module Main where

import System.Exit (exitSuccess)

import Env (initAppEnvData, runAppEnv)
import Configs (initConfigs)
import OutputHandles (initOutputHandles)
import Game (runGame)
import Graphics (initGraphics)
import InputState
import GameState
import GameState.Types

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    gr <- initGraphics tm outs
    inputs <- initInputState
    runAppEnv (initAppEnvData configs outs inputs gr $ New $ initGameState configs inputs gr) runGame
