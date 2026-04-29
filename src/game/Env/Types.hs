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
import Graphics.NewDraw
import GameState
import Graphics.Types
import SaveData

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
    , appEnvDataGameState :: !StateType
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
    readGameState :: AppEnv StateType
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

instance GameDataStorage AppEnv where
    newData = do
        cfgs <- readConfigs
        liftIO $ startNewGame cfgs

    saveData gd = liftIO $ saveToFile gd

    loadData fp = liftIO $ loadFromFile fp

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
    getAction = do
        gt <- readGameState
        case gt of
            Old _ -> return $ Step NoChange
            New gse -> do
                cfgs <- readConfigs
                inputs <- readInputState
                if wasWindowResized inputs
                    then return (Step ResizeWindow)
                    else return $ anyThink (gameStateE gse) cfgs inputs


    executeAction :: Action -> AppEnv (Maybe GameStep)
    executeAction (Exit _) = return Nothing
    executeAction (Step step) = return $ Just step
    executeAction (LoadSave fp sf) = do
        gdE <- loadData fp
        case gdE of
            Left err -> do
                liftIO $ putStrLn ("Failed to load " ++ fp ++ " : " ++ show err)
                return Nothing
            Right gsd -> return $ Just $ sf gsd
    executeAction (SaveData gd gs) = do
        saveData gd
        return $ Just gs
    executeAction (NewGame sf) = (Just . sf) <$> newData
    executeAction (SaveList sf) = undefined -- add later

    stepGame :: GameStep -> AppEnv ToRender
    stepGame step = do
        cfgs <- readConfigs
        inputs <- readInputState
        outs <- getOutputs
        gr <- readGraphics
        gt <- readGameState
        case gt of
            New gs -> do
                let gsn' = stepGameState cfgs inputs gr gs step
                modify $ \ae -> ae { appEnvDataGameState = New gsn' }
                return $ drawAssets gr $ gView gsn'
            Old gs -> do
                gs' <- updateGameState cfgs inputs outs gr gs
                modify $ \ae -> ae { appEnvDataGameState = Old gs' }
                updateWindow
