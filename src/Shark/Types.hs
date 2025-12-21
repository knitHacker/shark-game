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
    , Boat(..)    , checkPlayConfigs
    , StartMechanics(..)
    , EncounterMechanics(..)
    , Fundraisers(..)
    , FundraisingMechanics(..)
    , GameMechanics(..)
    , SiteLocation(..)
    , RegionInformation(..)
    , FullLocation(..)
    , EquipInfoType(..)
    , SharkEncounterRate(..)
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON(..), ToJSON(..), eitherDecodeFileStrict, encodeFile, Value(..), object, (.:), (.=) )
import Data.Aeson.Types ( Parser )
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Util

data EquipInfoType = Caught | Observed
    deriving (Generic, Show, Eq, Ord)

instance FromJSON EquipInfoType where
    parseJSON = \v -> do
        s <- parseJSON v
        case (s :: T.Text) of
            "caught" -> return Caught
            "observed" -> return Observed
            _ -> fail "EquipInfoType must be 'caught' or 'observed'"

instance ToJSON EquipInfoType where
    toJSON Caught = String "caught"
    toJSON Observed = String "observed"

data SharkEncounterRate = SharkEncounterRate
    { caughtRate :: Int
    , observedRate :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON SharkEncounterRate where
    parseJSON = \_ -> fail "SharkEncounterRate should not be parsed directly"

instance ToJSON SharkEncounterRate where
    toJSON (SharkEncounterRate caught observed) = toJSON [caught, observed]

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

data Fundraisers = Fundraisers
    { fundraiserName :: T.Text
    , fundraiserDescription :: T.Text
    , fundraiserBaseAmount :: Int
    , fundraiserCost :: Int
    , fundraiserSuccessRate :: Int
    , fundraiserRecurring :: Bool
    } deriving (Generic, Show, Eq)

instance FromJSON Fundraisers
instance ToJSON Fundraisers

data FundraisingMechanics = Fundraising
    { fundraisers :: M.Map T.Text Fundraisers
    } deriving (Generic, Show, Eq)

instance FromJSON FundraisingMechanics
instance ToJSON FundraisingMechanics

data GameMechanics = GameMech
    { mechanicsStart :: StartMechanics
    , mechanicsEncounter :: EncounterMechanics
    , mechanicsFundraising :: FundraisingMechanics
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
    , equipInfoType :: EquipInfoType
    , equipEffectiveness :: Int
    , equipImage :: T.Text
    } deriving (Generic, Show, Eq)

instance FromJSON GameEquipment
instance ToJSON GameEquipment

checkGameLocation :: GameLocation -> Bool
checkGameLocation loc = (totalCaught == 100) && (totalObserved == 100)
    where
        rates = M.elems $ sharksFound loc
        totalCaught = sum $ caughtRate <$> rates
        totalObserved = sum $ observedRate <$> rates

data GameLocation = GameLoc
    { showText :: T.Text
    , biomeDescription :: T.Text
    , allowedEquipment :: [T.Text]
    , sharksFound :: M.Map T.Text SharkEncounterRate
    } deriving (Generic, Show, Eq)

instance FromJSON GameLocation where
    parseJSON = \v -> do
        obj <- parseJSON v
        showTxt <- obj .: "showText"
        desc <- obj .: "biomeDescription"
        equip <- obj .: "allowedEquipment"
        sharksArr <- obj .: "sharksFound" :: Parser [[Value]]
        let parseShark [nameVal, caughtVal, observedVal] = do
                name <- parseJSON nameVal
                caught <- parseJSON caughtVal
                observed <- parseJSON observedVal
                return (name, SharkEncounterRate caught observed)
            parseShark _ = fail "sharksFound array must have format [name, caughtRate, observedRate]"
        sharksList <- mapM parseShark sharksArr
        return $ GameLoc showTxt desc equip (M.fromList sharksList)

instance ToJSON GameLocation where
    toJSON (GameLoc showTxt desc equip sharks) =
        object [ "showText" .= showTxt
               , "biomeDescription" .= desc
               , "allowedEquipment" .= equip
               , "sharksFound" .= ((\(name, SharkEncounterRate c o) -> toJSON [toJSON name, toJSON c, toJSON o]) <$> M.toList sharks)
               ]

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
    { dataType :: EquipInfoType
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
