{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( updateWindow
    ) where


import Foreign.C.Types ( CInt )
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Text as T

import Configs
import GameState
import GameState.Types
import OutputHandles.Types
import OutputHandles.Draw
import OutputHandles.Util
import OutputHandles.Images

import Debug.Trace
import Graphics.Draw
import Graphics.Types
import Graphics.Menu

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m, GraphicsRead m) => m ToRender
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    gr <- readGraphics
    case gs of
        Old gs' -> return $ updateWindow' gr (gameView gs')
        _ -> undefined


updateWindow' :: Graphics -> GameView -> ToRender
updateWindow' gr (GameView v oM _ mM) = updateGameView gr 0 v <> mr <> ovr
    where
        ovr = case oM of
            Just (OverlayView True ov om) -> updateOverlayMenu gr 2 om <> updateGameView gr 3 ov
            _ -> renderEmpty
        mr = case mM of
            Just m -> updateGameMenu gr 1 m
            _ -> renderEmpty
