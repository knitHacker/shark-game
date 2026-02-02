{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Capabilities
    ( ConfigsRead(..)
    , InputRead(..)
    , OutputRead(..)
    , GameStateRead(..)
    , FileIO(..)
    , StateIO(..)
    , RandomGen(..)
    , TimeRead(..)
    ) where

import qualified Data.Text as T
import Data.Word (Word32)
import System.Random.MWC (Seed)
import Data.Time.Clock.System (SystemTime)

import Configs (GameConfigs(..), SettingConfigs(..))
import InputState.Types (InputState)
import OutputHandles.Types (OutputHandles)
import GameState.Types (GameState)
import GameData.Types (GameData, StateConfig)
import GameData.Load (SaveFileInfo)

-- ============================================================================
-- Read-Only Capabilities (Existing)
-- ============================================================================

-- | Read-only access to game configuration
class Monad m => ConfigsRead m where
    readConfigs :: m GameConfigs

    readFrameRate :: m Word32
    readFrameRate = do
        cfgs <- readConfigs
        return $ frameRate $ settingCfgs cfgs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug $ settingCfgs cfgs

-- | Read-only access to input state
class Monad m => InputRead m where
    readInputState :: m InputState

-- | Read-only access to output handles
class Monad m => OutputRead m where
    getOutputs :: m OutputHandles

-- | Read-only access to game state
class Monad m => GameStateRead m where
    readGameState :: m GameState

-- ============================================================================
-- Effect Capabilities (New)
-- ============================================================================

-- | File I/O capability for game saves
-- Provides operations for saving, loading, and listing game save files
class Monad m => FileIO m where
    -- | Save game data to a file
    saveGameFile :: GameData -> m (Either T.Text ())

    -- | Load game data from a file path
    loadGameFile :: FilePath -> m (Either T.Text GameData)

    -- | List all available save files for the save selection UI
    listGameSaves :: m [SaveFileInfo]

-- | State/User settings capability
-- Provides operations for persisting user configuration and preferences
class Monad m => StateIO m where
    -- | Save state configuration (last save file, user preferences, etc.)
    saveState :: StateConfig -> m (Either T.Text ())

    -- | Load state configuration
    loadState :: m (Either T.Text StateConfig)

-- | Random generation capability (for initial seeds only)
-- Note: Pure deterministic random number generation uses GameData.Random functions
-- This capability is only for system randomness like initial seed generation
class Monad m => RandomGen m where
    -- | Generate a new random seed from system entropy
    generateSeed :: m Seed

-- | Time capability
-- Provides access to current system time
class Monad m => TimeRead m where
    -- | Get the current system time
    getCurrentTime :: m SystemTime
