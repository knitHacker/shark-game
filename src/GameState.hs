{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , isGameExiting
    , updateGameState
    ) where

import Control.Monad ()
import Configs ( ConfigsRead(readConfigs), GameConfigs )
import InputState
    ( enterJustPressed
    , Direction(DDown, DUp)
    ,InputRead(..)
    , InputState(inputRepeat, inputStateDirection)
    )
import GameState.Types
import GameState.Menu.MainMenu ( initMainMenu )
import OutputHandles.Types ( OutputHandles, OutputRead(..) )

import qualified Data.Text as T
import qualified SDL

import Utils ( randomValue )

import GameState.Menu
import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

initGameState :: GameConfigs -> OutputHandles -> IO GameState
initGameState cfgs outs = initMainMenu cfgs outs

randomPosition :: (MonadIO m) => Int -> Int -> Int -> Int ->  m (Int, Int)
randomPosition width height iW iH = do
    xPos <- randomValue 17 (width - iW)
    yPos <- randomValue 33 (height - iH)
    return (xPos, yPos)

isGameExiting :: GameState -> Bool
isGameExiting GameExiting = True
isGameExiting _ = False

updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    case gs of
        GameView mm m -> return $ updateGameStateInMenu mm m cfgs inputs outs
        OverlayMenu tm bm -> return $ updateGameStateInOverlay tm bm cfgs inputs outs
        _ -> return gs
