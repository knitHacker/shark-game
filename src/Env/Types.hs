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

import Control.Monad.Reader          (MonadReader, ReaderT, asks)
import Control.Monad
import Control.Monad.IO.Class        (MonadIO)




data AppEnvData = AppEnvData
    { appEnvDataConfigs :: GameConfigs
    , appEnvDataOutputHandles :: OutputHandles
    , appEnvDataInputState :: InputState
    , appEnvDataGameState :: GameState
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

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = asks appEnvDataInputState

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv GameConfigs
    readConfigs = asks appEnvDataConfigs

instance GameStateRead AppEnv where
    readGameState :: AppEnv GameState
    readGameState = asks appEnvDataGameState
