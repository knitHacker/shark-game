{-# LANGUAGE Strict #-}
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
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    appEnvData <- initAppEnvData tm configs outs
    time <- getSystemTime
    runGame appEnvData
