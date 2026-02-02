{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Env.Types
    ( AppEnvData(..)
    , AppEnv(..)
    ) where

import Configs (GameConfigs)
import OutputHandles.Types (OutputHandles)
import InputState (InputState)
import GameState.Types (GameState)
import Graphics.Types
import Capabilities
import GameData.Save (saveToFile, saveStateConfig)
import GameData.Load (loadFromFile, loadStateConfig, listSaveFiles)

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import System.Random.MWC (createSystemRandom, save)
import Data.Time.Clock.System (getSystemTime)


data AppEnvData = AppEnvData
    { appEnvDataConfigs :: !GameConfigs
    , appEnvDataOutputHandles :: !OutputHandles
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

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = asks appEnvDataInputState

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv GameConfigs
    readConfigs = asks appEnvDataConfigs

instance GameStateRead AppEnv where
    readGameState :: AppEnv GameState
    readGameState = asks appEnvDataGameState

-- ============================================================================
-- Effect Capabilities
-- ============================================================================

instance FileIO AppEnv where
    saveGameFile gd = liftIO $ do
        result <- try $ saveToFile gd
        return $ case result of
            Left (e :: SomeException) -> Left $ T.pack $ show e
            Right () -> Right ()

    loadGameFile path = liftIO $ loadFromFile path

    listGameSaves = liftIO listSaveFiles

instance StateIO AppEnv where
    saveState sc = liftIO $ do
        result <- try $ saveStateConfig sc
        return $ case result of
            Left (e :: SomeException) -> Left $ T.pack $ show e
            Right () -> Right ()

    loadState = liftIO loadStateConfig

instance RandomGen AppEnv where
    generateSeed = liftIO $ createSystemRandom >>= System.Random.MWC.save

instance TimeRead AppEnv where
    getCurrentTime = liftIO getSystemTime

-- ============================================================================
-- IO Capability Instances (for initialization before AppEnv exists)
-- ============================================================================

instance FileIO IO where
    saveGameFile gd = do
        result <- try $ saveToFile gd
        return $ case result of
            Left (e :: SomeException) -> Left $ T.pack $ show e
            Right () -> Right ()

    loadGameFile path = loadFromFile path

    listGameSaves = listSaveFiles

instance StateIO IO where
    saveState sc = do
        result <- try $ saveStateConfig sc
        return $ case result of
            Left (e :: SomeException) -> Left $ T.pack $ show e
            Right () -> Right ()

    loadState = loadStateConfig

instance RandomGen IO where
    generateSeed = createSystemRandom >>= System.Random.MWC.save

instance TimeRead IO where
    getCurrentTime = getSystemTime
