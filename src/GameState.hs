{-# LANGUAGE Strict #-}
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
import GameState.Menu.GameMenus ( initMainMenu )
import OutputHandles.Types ( OutputHandles, OutputRead(..) )

import qualified Data.Text as T
import qualified SDL

import GameState.Menu
import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

initGameState :: GameConfigs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    gv <- initMainMenu cfgs outs
    return $ reDraw gv

isGameExiting :: GameState -> Bool
isGameExiting (GameState (GameExiting _) _ _) = True
isGameExiting _ = False

updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    gsM <- case gameView gs of
        GameView mm m -> return $ updateGameStateInMenu mm m cfgs inputs outs
        OverlayMenu tm bm -> return $ updateGameStateInOverlay tm bm cfgs inputs outs
        _ -> return Nothing
    case gsM of
        Nothing -> return $ noUpdate gs
        Just gv -> return $ reDraw gv
