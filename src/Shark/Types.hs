{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Shark.Types
    ( GameEquipment(..)
    , GameLocation(..)
    , PlayConfigs(..)
    , SharkInfo(..)
    , SharkFind(..)
    , TripAttempt(..)
    , TripState(..)
    , TripInfo(..)
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Util

data TripAttempt = TripAttempt
    { attemptMonth :: Int
    , attemptEqipment :: DataEntry GameEquipment
    }

data TripState = TripState
    { trip :: TripInfo
    , tripTries :: [TripAttempt]
    , tripTotalTries :: Int
    , sharkFinds :: [SharkFind]
    }

data GameEquipment = GameEquip
    { text :: T.Text
    , timeAdded :: Int
    , price :: Int
    , infoType :: T.Text
    , effectiveness :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON GameEquipment
instance ToJSON GameEquipment

data GameLocation = GameLoc
    { showText :: T.Text
    , requiredEquipment :: [T.Text]
    , allowedEquipment :: [T.Text]
    , sharksFound :: [T.Text]
    } deriving (Generic, Show, Eq)

instance FromJSON GameLocation
instance ToJSON GameLocation


data SharkInfo = SharkInfo
    { sharkName :: T.Text
--    , research :: M.Map T.Text SharkResearch
    } deriving (Generic, Show, Eq)

instance FromJSON SharkInfo
instance ToJSON SharkInfo

data PlayConfigs = PlayConfigs
    { equipment :: M.Map T.Text GameEquipment
    , siteLocations :: M.Map T.Text GameLocation
    , sharks :: M.Map T.Text SharkInfo
    } deriving (Generic, Show, Eq)

instance FromJSON PlayConfigs
instance ToJSON PlayConfigs

data TripInfo = TripInfo
    { tripDestination :: DataEntry GameLocation
    , tripEquipment :: [DataEntry GameEquipment]
    }

data SharkFind = SharkFind
    { findLocation :: T.Text
    , findSpecies :: T.Text
    , findDataType :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON SharkFind
instance ToJSON SharkFind

