{-# LANGUAGE Strict #-}
module Game
    ( runGame
    ) where

import OutputHandles.Types
import Env
import Env.Types
import InputState
import GameState.Types
import GameState.Draw
import GameState ( drawOverlay )
import Graphics.Types ( GraphicsRead(..) )
import Configs
import SaveData ( GameDataStorage )

import Data.Word ( Word32 )

-- Time for a frame
frameTime :: Word32 -> Word32
frameTime fps = div 1000 (fps + 1)

-- Game loop that enforces a frame rate throttling
runGame :: (Monad m, ConfigsRead m, InputRead m, GraphicsRead m, RendererActions m, GameDataStorage m, GameStateStep m) => GameState -> m ()
runGame gameState = do
    ft <- applyConfigs (frameRate . settingCfgs) frameTime
    inputRes <- pollInputState ft
    case inputRes of
        (InputResult _ _ True) -> cleanup
        (InputResult _ (Just rs) _) -> do
            resizeGraphics rs
            doGame inputRes gameState
        (InputResult _ _ _) -> doGame inputRes gameState


doGame :: (Monad m, ConfigsRead m, InputRead m, GraphicsRead m, RendererActions m, GameDataStorage m, GameStateStep m) => InputResult -> GameState -> m ()
doGame inputRes gs = do
    up <- getUpdate inputRes gs
    stepM <- executeAction up
    case stepM of
        Nothing -> cleanup
        (Just step) -> do
            gsM <- stepGame gs step
            case gsM of
                Nothing -> do
                    -- redraw to keep graphics fresh
                    drawGame gs
                    runGame gs
                (Just gs') -> do
                    drawGame gs'
                    runGame gs'


-- TODO: completely ignoring the cached render currently. eventually would be good to use it
drawGame :: (GraphicsRead m, RendererActions m) => GameState -> m ()
drawGame gs = do
    gr <- readGraphics
    let gv = case gameOverlay gs of
                Just ov -> (gameView gs) { viewOverlay = Just $ drawOverlay ov gr }
                Nothing -> (gameView gs) { viewOverlay = Nothing }
    executeDraw (renderGameView gr gv)

cleanup :: (RendererActions m) => m ()
cleanup = cleanupRenderer
