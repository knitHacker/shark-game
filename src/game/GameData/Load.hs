{-# LANGUAGE OverloadedStrings #-}

module GameData.Load
    ( SaveFileInfo(..)
    , listSaveFiles
    , formatSaveFileInfo
    , formatSaveFileInfoCompact
    , convertFromSave
    , loadFromFile
    , loadStateConfig
    ) where

import System.Directory (listDirectory, getModificationTime, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeBaseName)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Default (def)
import Control.Exception (catch, SomeException)
import qualified Data.Text as T
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Aeson (eitherDecodeFileStrict)
import System.Random.MWC (toSeed)
import qualified Data.List as L

import Env.Files (getLocalGamePath)
import GameData.Types
import GameData.Util (sortSharks)

-- ============================================================================
-- Save File Discovery
-- ============================================================================

data SaveFileInfo = SaveFileInfo
    { saveFileName :: T.Text
    , saveFilePath :: FilePath
    , saveFileTime :: UTCTime
    } deriving (Show, Eq)

-- | List all save files in the saves directory, sorted by modification time (newest first)
listSaveFiles :: IO [SaveFileInfo]
listSaveFiles = do
    savesDir <- getLocalGamePath ("data" </> "saves")
    dirExists <- doesDirectoryExist savesDir
    if not dirExists
        then return []
        else do
            files <- listDirectory savesDir `catch` handleError
            saveInfos <- mapM (getSaveFileInfo savesDir) (filter isSaveFile files)
            return $ sortOn (Down . saveFileTime) saveInfos
  where
    isSaveFile :: FilePath -> Bool
    isSaveFile fp = takeExtension fp == ".save"

    handleError :: SomeException -> IO [FilePath]
    handleError _ = return []

-- | Get metadata for a specific save file
getSaveFileInfo :: FilePath -> FilePath -> IO SaveFileInfo
getSaveFileInfo saveDir fileName = do
    let fullPath = saveDir </> fileName
    modTime <- getModificationTime fullPath
    let displayName = T.pack $ takeBaseName fileName  -- Remove .save extension
    return $ SaveFileInfo displayName fullPath modTime

-- | Format save file info for display in menus (full format)
formatSaveFileInfo :: SaveFileInfo -> T.Text
formatSaveFileInfo sfi =
    saveFileName sfi <> " - " <>
    T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (saveFileTime sfi))

-- | Format save file info for display (compact format)
formatSaveFileInfoCompact :: SaveFileInfo -> T.Text
formatSaveFileInfoCompact sfi =
    saveFileName sfi <> " (" <>
    T.pack (formatTime defaultTimeLocale "%m/%d %H:%M" (saveFileTime sfi)) <> ")"

-- ============================================================================
-- Pure Conversion Functions
-- ============================================================================

-- | Convert loaded save data back to GameData
convertFromSave :: FilePath -> GameSaveData -> GameData
convertFromSave filePath gsd =
    GameData
        { gameDataSaveFile = filePath
        , gameDataSeed = toSeed $ saveSeed gsd
        , gameDataFoundationNames = saveFoundationNames gsd
        , gameDataDonorList = saveDonorList gsd
        , gameDataFunds = saveFunds gsd
        , gameDataMonth = saveMonth gsd
        , gameDataSharkIndex = saveSharkIndex gsd
        , gameDataSharks = saveSharks gsd
        , gameDataFoundSharks = sortSharks $ saveSharks gsd  -- Rebuild species index
        , gameDataResearchComplete = saveResearchComplete gsd
        , gameDataEquipment = saveDataEquipment gsd
        , gameCurrentRegion = saveCurrentRegion gsd
        }

-- ============================================================================
-- IO Load Operations
-- ============================================================================

-- | Load GameData from a JSON file
loadFromFile :: FilePath -> IO (Either T.Text GameData)
loadFromFile filePath = do
    result <- eitherDecodeFileStrict filePath
    case result of
        Left err -> return $ Left $ T.pack ("Failed to open save file: " L.++ show err)
        Right gameSaveData -> return $ Right $ convertFromSave filePath gameSaveData

-- | Load StateConfig from state.yaml, or return default if not found
loadStateConfig :: IO (Either T.Text StateConfig)
loadStateConfig = do
    configPath <- getLocalGamePath ("data" </> "configs" </> "state.yaml")
    result <- (eitherDecodeFileStrict configPath `catch` handleMissing)
    return $ case result of
        Left err -> Right def  -- Return default config if file doesn't exist or has errors
        Right stateConfig -> Right stateConfig
  where
    handleMissing :: SomeException -> IO (Either String StateConfig)
    handleMissing _ = return $ Right def
