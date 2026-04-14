{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( renderGameView
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict ((!))

import Configs
import Graphics.Types (GraphicsRead(..))
import GameState.Types
import OutputHandles.Types
import OutputHandles.Draw
import OutputHandles.Util
import OutputHandles.Images

import Graphics.Draw
import Graphics.Types
import Graphics.Menu


renderGameView :: Graphics -> GameView -> ToRender
renderGameView gr (GameView v oM mM) =
    updateGameView gr 0 v <> menuRender <> overlayRender
    where
        menuRender = case mM of
            Just (ViewMenu m) -> updateGameMenu gr 1 m
            Nothing           -> renderEmpty
        overlayRender = case oM of
            Just (OverlayView True ov om) ->
                updateOverlayMenu gr 2 om <> updateGameView gr 3 ov
            _ -> renderEmpty
