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
import GameState.Types
import SaveData
import Configs

import qualified SDL
import Control.Monad.IO.Class ()
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Time.Clock.System
    ( SystemTime(systemSeconds, systemNanoseconds)
    , getSystemTime
    )
import Data.Word ( Word32 )
import Control.Concurrent
import Control.Arrow

import Debug.Trace

-- Game loop that enforces a frame rate throttling
runGame :: (Monad m, ConfigsRead m, InputUpdate m, GraphicsRead m) => m ()
runGame = do
    -- instance of InputUpdate decides how to get new input object
    input' <- updateInputState
    if inputStateQuit input'
        then return endGame
        else applyInputs input'


applyInputs :: (Monad m, InputRead m) => InputState -> m ()
applyInputs input' = do
    _ <- updateWindow (windowResizeed input)
    action <- getAction
    stepM <- executeAction action
    case stepM of
        Nothing -> return endGame
        Just step -> do
            render <- stepGame step
            drawRender render
            runGame

endGame :: (Monad m, RenderAction m) => m ()
endGame = cleanupRenderer
