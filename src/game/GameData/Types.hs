{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GameData.Types
    ( -- * Game Data Types
      GameData(..)
    , GameSaveData(..)
    , GameSharkData(..)
    , ResearchCompleteInfo(..)
    , GameDataEquipment(..)
    , StateConfig(..)
    , SharkIndex
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Vector.Unboxed ( Vector )
import Data.Word ( Word32 )
import System.Random.MWC ( Seed )
import Data.Default ( Default(..) )

-- | Index for shark data in the game
type SharkIndex = Int

-- | Equipment data for the player
data GameDataEquipment = GameEquipment
    { gameActiveBoat :: T.Text
    , gameOwnedBoats :: [T.Text]
    , gameOwnedEquipment :: [T.Text]
    } deriving (Show, Eq, Generic)

instance FromJSON GameDataEquipment
instance ToJSON GameDataEquipment

-- | Individual shark encounter data
data GameSharkData = GameShark
    { gameSharkMonth :: Int
    , gameSharkSpecies :: T.Text
    , gameSharkRegion :: T.Text
    , gameSharkLocation :: T.Text
    , gameSharkEquipment :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON GameSharkData
instance ToJSON GameSharkData

-- | Research completion tracking
data ResearchCompleteInfo = ResearchCompleteInfo
    { researchCompleteMonth :: Int
    , researchCompleteSharks :: [SharkIndex]
    } deriving (Show, Eq, Generic)

instance FromJSON ResearchCompleteInfo
instance ToJSON ResearchCompleteInfo

-- | Main game state data
data GameData = GameData
    { gameDataSaveFile :: String
    , gameDataSeed :: Seed
    , gameDataFoundationNames :: [T.Text]
    , gameDataDonorList :: [T.Text]
    , gameDataFunds :: Int
    , gameDataMonth :: Int
    , gameDataSharkIndex :: SharkIndex
    , gameDataSharks :: M.Map SharkIndex GameSharkData
    , gameDataFoundSharks :: M.Map T.Text [SharkIndex]
    , gameDataResearchComplete :: M.Map T.Text ResearchCompleteInfo
    , gameDataEquipment :: GameDataEquipment
    , gameCurrentRegion :: T.Text
    } deriving (Show, Eq)

-- | Serializable game save data (Seed converted to Vector Word32)
data GameSaveData = GameSaveData
    { saveSeed :: Vector Word32
    , saveFoundationNames :: [T.Text]
    , saveDonorList :: [T.Text]
    , saveFunds :: Int
    , saveMonth :: Int
    , saveSharkIndex :: SharkIndex
    , saveSharks :: M.Map SharkIndex GameSharkData
    , saveResearchComplete :: M.Map T.Text ResearchCompleteInfo
    , saveDataEquipment :: GameDataEquipment
    , saveCurrentRegion :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON GameSaveData
instance ToJSON GameSaveData

-- | User state and settings (separate from game data, persists across games)
data StateConfig = StateConfig
    { lastSaveFile :: Maybe FilePath
    } deriving (Show, Eq, Generic)

instance FromJSON StateConfig
instance ToJSON StateConfig

-- | Default empty state config
instance Default StateConfig where
    def = StateConfig
        { lastSaveFile = Nothing
        }
