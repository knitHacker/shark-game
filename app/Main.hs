module Main where

import System.Exit (exitSuccess)

import Env (initAppEnvData)
import Configs (initConfigs)
import OutputHandles (initOutputHandles)
import Game (runGame)


main :: IO ()
main = do
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    gr <- initGraphics tm outs
    gs <- 
    appEnvData <- initAppEnvData tm configs outs
    runGame appEnvData
