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
    , Boat(..)
    , checkPlayConfigs
    , StartMechanics(..)
    , EncounterMechanics(..)
    , GameMechanics(..)
    , SiteLocation(..)
    , RegionInformation(..)
    , FullLocation(..)
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Util

data StartMechanics = Mechanics
    { startingFunds :: Int
    , startingBoat :: T.Text
    , startingEquipment :: [T.Text]
    , startingRegion :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON StartMechanics
instance ToJSON StartMechanics

data EncounterMechanics = Encounter
    { baseAttempts :: Int
    , bonusAttemptChancePercent :: Int
    , bonusMaxAttempts :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON EncounterMechanics
instance ToJSON EncounterMechanics

data GameMechanics = GameMech
    { mechanicsStart :: StartMechanics
    , mechanicsEncounter :: EncounterMechanics
    } deriving (Generic, Show, Eq)

instance FromJSON GameMechanics
instance ToJSON GameMechanics

data Boat = Boat
    { boatName :: T.Text
    , boatDescription :: T.Text
    , boatReachableBiomes :: [T.Text]
    , boatEquipmentSlots :: Int
    , boatFuelCost :: Int
    , boatPrice :: Int
    , boatImage :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON Boat
instance ToJSON Boat

data TripAttempt = TripAttempt
    { attemptMonth :: Int
    , attemptEqipment :: DataEntry T.Text GameEquipment
    } deriving (Show, Eq)

data TripState = TripState
    { trip :: TripInfo
    , tripTries :: [TripAttempt]
    , tripTotalTries :: Int
    , sharkFinds :: [SharkFind]
    } deriving (Show, Eq)

data GameEquipment = GameEquip
    { equipText :: T.Text
    , equipSize :: Int
    , equipTimeAdded :: Int
    , equipPrice :: Int
    , equipInfoType :: T.Text
    , equipEffectiveness :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON GameEquipment
instance ToJSON GameEquipment

checkGameLocation :: GameLocation -> Bool
checkGameLocation loc = foldl (\p (_, c) -> p + c) 0 shks == 100
    where
        shks = sharksFound loc

data GameLocation = GameLoc
    { showText :: T.Text
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

data RegionInformation = RegionInformation
    { regionShowText :: T.Text
    , regionDescription :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON RegionInformation
instance ToJSON RegionInformation

data SiteLocation = SiteLocation
    { regionInfo :: RegionInformation
    , siteLocations :: M.Map T.Text GameLocation
    } deriving (Generic, Show, Eq)

instance FromJSON SiteLocation
instance ToJSON SiteLocation

data FullLocation = FullLocation
    { locationRegion :: RegionInformation
    , locationSite :: GameLocation
    } deriving (Generic, Show, Eq)

instance FromJSON FullLocation
instance ToJSON FullLocation

checkPlayConfigs :: PlayConfigs -> Bool
checkPlayConfigs cfgs = and $ checkGameLocation <$> sitesLoc
    where
        sitesLoc = concatMap (M.elems . siteLocations) $ M.elems (regions cfgs)

data PlayConfigs = PlayConfigs
    { boats :: M.Map T.Text Boat
    , equipment :: M.Map T.Text GameEquipment
    , regions :: M.Map T.Text SiteLocation
    , sharks :: M.Map T.Text SharkInfo
    , research :: M.Map T.Text ResearchData
    , gameMechanics :: GameMechanics
    } deriving (Generic, Show, Eq)

instance FromJSON PlayConfigs
instance ToJSON PlayConfigs

data TripInfo = TripInfo
    { tripDestination :: DataEntry (T.Text, T.Text) FullLocation
    , tripEquipment :: [DataEntry T.Text GameEquipment]
    , tripBoat :: DataEntry T.Text Boat
    } deriving (Show, Eq)

data SharkFind = SharkFind
    { findMonth :: Int
    , findSpecies :: DataEntry T.Text SharkInfo
    , findLocation :: DataEntry (T.Text, T.Text) FullLocation
    , findEquipment :: DataEntry T.Text GameEquipment
    } deriving (Generic, Show, Eq)

instance FromJSON SharkFind
instance ToJSON SharkFind
