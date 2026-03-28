module Main where

import System.Exit (exitSuccess)

import Env (initAppEnvData)
import Configs (initConfigs)
import OutputHandles (initOutputHandles)
import Game (runGame)


main :: IO ()
main = do
    confE <- initConfigs
    case confE of
        Left err -> "Can't load"
        Right (tm, configs) -> do
            outs <- initOutputHandles tm configs
            appEnvData <- initAppEnvData tm configs outs
            runGame appEnvData
