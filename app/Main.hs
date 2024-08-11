{-# LANGUAGE OverloadedStrings #-}
module Main where

import Env
import Configs
import OutputHandles
import Game

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified SDL
import Data.Time.Clock.System


main :: IO ()
main = do
    configs <- initConfigs
    outs <- initOutputHandles configs
    appEnvData <- initAppEnvData (gameCfgs configs) outs
    time <- getSystemTime
    runGame 0 time appEnvData
