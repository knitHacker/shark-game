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

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m ToRender
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    case gameLastDraw gs of
        Just r -> return r
        Nothing -> return $ updateWindow' (gameGraphics gs) (gameView gs)


updateWindow' :: Graphics -> GameView -> ToRender
updateWindow' gr gv =
    case gv of
        (GameMenu _ m) -> updateGameMenu gr 0 m
        (OverlayMenu (Overlay x y w h c m) back) ->
            let backR = updateBasicView gr 0 back
                bg = addRectangle renderEmpty 1 0 (DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
                frontR = updateGameMenu gr 2 m
            in backR <> bg <> frontR
        (GameTimeout _ tov) -> updateTimeoutView gr 0 tov
        _ -> renderEmpty
