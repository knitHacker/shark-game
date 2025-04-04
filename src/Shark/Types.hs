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
    , ResearchReq(..)
    , ResearchData(..)
    , checkPlayConfigs
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

checkGameLocation :: GameLocation -> Bool
checkGameLocation loc = foldl (\p (_, c) -> p + c) 0 shks == 100
    where
        shks = sharksFound loc

data GameLocation = GameLoc
    { showText :: T.Text
    , requiredEquipment :: [T.Text]
    , allowedEquipment :: [T.Text]
    , sharksFound :: [(T.Text, Int)]
    } deriving (Generic, Show, Eq)

instance FromJSON GameLocation
instance ToJSON GameLocation


data SharkFact = SharkFact
    { sharkFactTitle :: T.Text
    , sharkFactInfo :: T.Text
    , sharkFactRevealDepends :: [T.Text]
    } deriving (Generic, Show, Eq)

instance FromJSON SharkFact
instance ToJSON SharkFact

data SharkInfo = SharkInfo
    { sharkName :: T.Text
    , sharkImage :: T.Text
    , sharkFacts :: [SharkFact]
    } deriving (Generic, Show, Eq)

instance FromJSON SharkInfo
instance ToJSON SharkInfo

data ResearchReq = ResearchReq
    { dataType :: T.Text
    , reqCount :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON ResearchReq
instance ToJSON ResearchReq

data ResearchData = ResearchData
    { researchReqs :: M.Map T.Text [ResearchReq]
    , researchPaperName :: T.Text
    , researchDescription :: T.Text
    , researchGrant :: Int
    , researchDepends :: [T.Text]
    } deriving (Generic, Show, Eq)

instance FromJSON ResearchData
instance ToJSON ResearchData

checkPlayConfigs :: PlayConfigs -> Bool
checkPlayConfigs cfgs = and $ checkGameLocation <$> siteLocations cfgs

data PlayConfigs = PlayConfigs
    { equipment :: M.Map T.Text GameEquipment
    , siteLocations :: M.Map T.Text GameLocation
    , sharks :: M.Map T.Text SharkInfo
    , research :: M.Map T.Text ResearchData
    } deriving (Generic, Show, Eq)

instance FromJSON PlayConfigs
instance ToJSON PlayConfigs

data TripInfo = TripInfo
    { tripDestination :: DataEntry GameLocation
    , tripEquipment :: [DataEntry GameEquipment]
    }

data SharkFind = SharkFind
    { findMonth :: Int
    , findSpecies :: DataEntry SharkInfo
    , findLocation :: DataEntry GameLocation
    , findEquipment :: DataEntry GameEquipment
    } deriving (Generic, Show, Eq)

instance FromJSON SharkFind
instance ToJSON SharkFind

