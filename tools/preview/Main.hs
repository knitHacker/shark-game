module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import System.Environment (getArgs)

import Configs
import Env
import Env.Types
import GameState
import GameState.Types
import GameState.Draw
import Graphics
import InputState
import OutputHandles
import OutputHandles.Types
import SaveData

import Generate

main :: IO ()
main = do
    putStrLn "Preview"
