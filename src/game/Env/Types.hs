{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env.Types
    ( AppEnvData(..)
    , AppEnv(..)
    ) where

import Configs
import OutputHandles
import OutputHandles.Types
import InputState
import GameState.Types
import GameState.Draw
import GameState
import Graphics.Types

-- import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.State.Strict (StateT, MonadState, gets, modify)
import Control.Monad
import Control.Monad.IO.Class (MonadIO)

import Data.Word (Word32)
import Control.Monad.IO.Class (liftIO)


-- Time for a frame
frameTime :: Word32 -> Word32
frameTime fps = div 1000 (fps + 1)


-- Have a InputState / GameState stepped and updated every step
data AppEnvData = AppEnvData
    { appEnvDataConfigs :: !GameConfigs
    , appEnvDataOutputHandles :: !OutputHandles
    , appEnvDataInputState :: !InputState
    , appEnvDataGraphics :: !Graphics
    , appEnvDataGameState :: !GameState
    }


newtype AppEnv a = AppEnv (StateT AppEnvData IO a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState AppEnvData
        , MonadIO
        )

instance OutputRead AppEnv where
    getOutputs :: AppEnv OutputHandles
    getOutputs = gets appEnvDataOutputHandles

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv GameConfigs
    readConfigs = gets appEnvDataConfigs

instance GraphicsRead AppEnv where
    readGraphics :: AppEnv Graphics
    readGraphics = gets appEnvDataGraphics

instance GameStateRead AppEnv where
    readGameState :: AppEnv GameState
    readGameState = gets appEnvDataGameState

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = gets appEnvDataInputState

instance InputUpdate AppEnv where
    updateInputState :: AppEnv InputState
    updateInputState = do
        fr <- readFrameRate
        old <- readInputState
        new <- updateInput (frameTime fr) old
        modify $ \ae -> ae { appEnvDataInputState = new }
        return new

instance GraphicsUpdate AppEnv where
    updateWindowSize Nothing = return ()
    updateWindowSize (Just (w, h)) = do
        gr <- readGraphics
        let gr' = gr { graphicsWindowWidth = w, graphicsWindowHeight = h}
        modify $ \ae -> ae { appEnvDataGraphics = gr' }

    updateFont fs = do
        gr <- readGraphics
        let gr' = gr { graphicsFontSize = fs }
        modify $ \ae -> ae { appEnvDataGraphics = gr' }

instance RenderAction AppEnv where
    drawRender :: ToRender -> AppEnv ()
    drawRender rend = executeDraw rend

    cleanupRenderer :: AppEnv ()
    cleanupRenderer = do
        outs <- getOutputs
        liftIO $ cleanupOutputHandles outs

instance GameStateStep AppEnv where
    getAction :: AppEnv Action
    getAction = return $ Step NoChange

    executeAction :: Action -> AppEnv (Maybe GameStep)
    executeAction (Exit _) = return Nothing
    executeAction (Step step) = return $ Just step
    executeAction _ = undefined

    stepGame :: GameStep -> AppEnv ToRender
    stepGame _ = do
        gs' <- updateGameState
        modify $ \ae -> ae { appEnvDataGameState = gs' }
        updateWindow