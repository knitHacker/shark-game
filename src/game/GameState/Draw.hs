{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( updateWindow
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict ((!))

import Configs
import Graphics (GraphicsRead(..))
import GameState.Types
import OutputHandles.Types
import OutputHandles.Draw
import OutputHandles.Util
import OutputHandles.Images

import Graphics.Draw
import Graphics.Types
import Graphics.Menu

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m, GraphicsRead m) => m ToRender
updateWindow = do
    gs   <- readGameState
    gr   <- readGraphics
    cfgs <- readConfigs
    case gameLastDraw gs of
        Just r  -> return r
        Nothing ->
            let gv = anyDraw (gameCurrentState gs) gr cfgs
            in return $ renderGameView gr gv


renderGameView :: Graphics -> GameView -> ToRender
renderGameView gr (GameView v oM mM) =
    updateGameView gr 0 v <> menuRender <> overlayRender
    where
        menuRender = case mM of
            Just m -> updateGameMenu gr 1 m
            Nothing -> renderEmpty
        overlayRender = case oM of
            Just (OverlayView True ov om) ->
                updateOverlayMenu gr 2 om <> updateGameView gr 3 ov
            _ -> renderEmpty
