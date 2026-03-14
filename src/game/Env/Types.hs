{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Env.Types
    ( AppEnv(..)
    , GameCmd(..)
    , GameEvent(..)
    ) where

import Configs (GameConfigs)
import Handles.Types
import InputState (InputState)
import GameState.Types (GameState)
import Graphics.Types
import GameData.Save (saveToFile, saveStateConfig)
import GameData.Load (loadFromFile, loadStateConfig, listSaveFiles)
import Handles

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import System.Random.MWC (createSystemRandom, save)
import Data.Time.Clock.System (getSystemTime)
import Data.Word (Word32)
import GameData.Types
import Draw.Types


data AppEnv = AppEnv
    { appEnvHandles :: !Handles
    , appEnvGameState :: !GameState
    , appEnvConfigs :: !GameConfigs
    }

data GameCmd = ExitGame
             | SaveGame GameData
             | SaveState StateConfig
             | LoadGame String
             | ListSaves
             | DrawFrame ToRender
             | RequestInput Word32

data GameEvent = SaveFile GameData
               | SaveFileNames [String]
               | InputUpdate InputState
