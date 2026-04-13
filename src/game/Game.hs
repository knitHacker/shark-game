{-# LANGUAGE Strict #-}
module Game
    ( runGame
    ) where

import OutputHandles.Types
import OutputHandles
import Env
import Env.Types
import InputState
import GameState
import GameState.Draw
import Graphics
import Configs

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
            doGame gameState
        (InputResult _ _ _) -> doGame gameState


doGame :: (Monad m, ConfigsRead m, InputRead m, GraphicsRead m, RendererActions m, GameDataStorage m, GameStateStep m) => GameState -> m ()
doGame gs = do
    up <- getUpdate gs
    stepM <- executeAction up
    case stepM of
        Nothing -> cleanup
        (Just step) -> do
            gsM <- stepGame gs step
            case gsM of
                Nothing -> runGame gs
                (Just gs') -> do
                    drawGame gs'
                    runGame gs'


drawGame :: (Monad m, ConfigsRead m, InputRead m, GraphicsRead m, RendererActions m, GameDataStorage m, GameStateStep m) => GameState -> m ()
drawGame (GameState _ gv _) = do
    gr <- readGraphics
    executeDraw (renderGameView gr gv)

cleanup :: (RendererActions m) => m ()
cleanup = cleanupRenderer
