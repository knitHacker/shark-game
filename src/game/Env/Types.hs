{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env.Types
    ( AppEnvData(..)
    , AppEnv(..)
    ) where

import Configs
import OutputHandles.Types
import InputState
import GameState.Types
import Graphics.Types

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import System.Directory


data AppEnvData = AppEnvData
    { appEnvDataConfigs :: !GameConfigs
    , appEnvDataOutputHandles :: !OutputHandles
    , appEnvDataGraphics :: !Graphics
    , appEnvDataInputState :: !InputState
    , appEnvDataGameState :: !GameState
    }


newtype AppEnv a = AppEnv (ReaderT AppEnvData IO a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppEnvData
        , MonadIO
        )

instance OutputRead AppEnv where
    getOutputs :: AppEnv OutputHandles
    getOutputs = asks appEnvDataOutputHandles

instance GraphicsRead AppEnv where
    readGraphics :: AppEnv Graphics
    readGraphics = asks appEnvDataGraphics

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = asks appEnvDataInputState

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv GameConfigs
    readConfigs = asks appEnvDataConfigs

instance GameStateRead AppEnv where
    readGameState :: AppEnv GameState
    readGameState = asks appEnvDataGameState

instance GameDataStorage AppEnv where
    saveData :: GameData -> AppEnv ()
    saveData gd = liftIO $ saveToFile gd

    loadData :: FilePath -> AppEnv (Either T.Text GameData)
    loadData fp =liftIO $ loadFromFile fp

    getSaveFiles :: AppEnv [FilePath]
    getSaveFiles = do
        localPath <- liftIO $ getSaveDir
        contents <- listDirectory localPath
