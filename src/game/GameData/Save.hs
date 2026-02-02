{-# LANGUAGE OverloadedStrings #-}

module GameData.Save
    ( convertToSave
    , saveToFile
    , saveStateConfig
    , startNewGame
    ) where

import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import System.Random.MWC (createSystemRandom, uniformR, save, fromSeed)
import Data.Aeson (encodeFile)
import qualified Data.Map.Strict as M

import GameData.Types
import GameData.File (makeSaveFilePath, maxRand)
import Configs (GameConfigs(..), sharkCfgs)
import Shark.Types
    ( GameMechanics(..)
    , StartMechanics(..)
    , gameMechanics
    , mechanicsStart
    , startingBoat
    , startingEquipment
    , startingFunds
    , startingRegion
    )
import Env.Files (getLocalGamePath)

-- ============================================================================
-- Pure Conversion Functions
-- ============================================================================

-- | Convert GameData to saveable format (FilePath, GameSaveData)
convertToSave :: GameData -> (FilePath, GameSaveData)
convertToSave gd =
    ( gameDataSaveFile gd
    , GameSaveData
        { saveSeed = fromSeed $ gameDataSeed gd
        , saveFoundationNames = gameDataFoundationNames gd
        , saveDonorList = gameDataDonorList gd
        , saveFunds = gameDataFunds gd
        , saveMonth = gameDataMonth gd
        , saveSharkIndex = gameDataSharkIndex gd
        , saveSharks = gameDataSharks gd
        , saveResearchComplete = gameDataResearchComplete gd
        , saveDataEquipment = gameDataEquipment gd
        , saveCurrentRegion = gameCurrentRegion gd
        }
    )

-- ============================================================================
-- IO Save Operations
-- ============================================================================

-- | Save GameData to a JSON file
saveToFile :: GameData -> IO ()
saveToFile gd = do
    let (filePath, gameSaveData) = convertToSave gd
    encodeFile filePath gameSaveData

-- | Save StateConfig to state.yaml
saveStateConfig :: StateConfig -> IO ()
saveStateConfig stateConfig = do
    configPath <- getLocalGamePath ("data" </> "configs" </> "state.yaml")
    createDirectoryIfMissing True (takeDirectory configPath)
    encodeFile configPath stateConfig

-- | Create a new game with random seed and save file
startNewGame :: GameConfigs -> IO GameData
startNewGame cfgs = do
    let startCfg = mechanicsStart $ gameMechanics $ sharkCfgs cfgs

    -- Generate random seed and filename
    gen <- createSystemRandom
    randomNum <- uniformR (0, maxRand) gen  -- Use maxRand as upper limit
    seed <- save gen

    -- Create save file path (uses makeFileName internally)
    filePath <- makeSaveFilePath randomNum

    -- Build initial game data
    let startBoat = startingBoat startCfg
        gameEquipment = GameEquipment startBoat [startBoat] (startingEquipment startCfg)
        startMoney = startingFunds startCfg
        gameData = GameData
            { gameDataSaveFile = filePath
            , gameDataSeed = seed
            , gameDataFoundationNames = []
            , gameDataDonorList = []
            , gameDataFunds = startMoney
            , gameDataMonth = 0
            , gameDataSharkIndex = 0
            , gameDataSharks = M.empty
            , gameDataFoundSharks = M.empty
            , gameDataResearchComplete = M.empty
            , gameDataEquipment = gameEquipment
            , gameCurrentRegion = startingRegion startCfg
            }

    -- Save the new game immediately
    saveToFile gameData
    return gameData
