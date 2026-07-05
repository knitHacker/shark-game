module Main where


import Env (initAppEnvData, runAppEnv)
import Configs (initConfigs)
import OutputHandles (initOutputHandles)
import Game (runGame)
import Graphics (initGraphics)
import InputState
import GameState


main :: IO ()
main = do
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    gr <- initGraphics tm outs
    inputs <- initInputState
    runAppEnv (initAppEnvData configs outs inputs gr $ initGameState configs inputs gr) runGame
