{-# LANGUAGE Strict #-}
module Game
    ( runGame
    ) where

import OutputHandles.Types
import InputState
import GameState.Types
import Configs
import Graphics.Types

import Debug.Trace

-- Game loop that enforces a frame rate throttling
runGame :: (Monad m, RenderAction m, GameStateStep m, ConfigsRead m, InputRead m, InputUpdate m, RenderAction m, GraphicsRead m, GraphicsUpdate m) => m ()
runGame = do
    -- instance of InputUpdate decides how to get new input object
    input' <- updateInputState
    if inputQuit input'
        then endGame
        else applyInputs input'


applyInputs :: (Monad m, RenderAction m, ConfigsRead m, GameStateStep m, InputRead m, InputUpdate m, GraphicsRead m, GraphicsUpdate m) => InputState -> m ()
applyInputs input' = do
    _ <- updateWindowSize (windowResized input')
    action <- getAction
    stepM <- executeAction action
    case stepM of
        Nothing -> endGame
        Just step -> do
            render <- stepGame step
            drawRender render
            runGame

endGame :: (Monad m, RenderAction m) => m ()
endGame = cleanupRenderer
