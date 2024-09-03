module Shark.Trip
    ( catchAttempts
    , executeTrip
    , tripCost
    , tripInfo
    , tripLength
    , initTripProgress
    ) where

import SaveData
import Configs
import Shark.Types
import Util

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List as L

tripInfo :: PlayConfigs -> T.Text -> [T.Text] -> TripInfo
tripInfo cfg loc eqTxt = TripInfo locE eqEs
    where
        locE = getEntry (siteLocations cfg) loc
        eqEs = (\k -> getEntry (equipment cfg) k) <$> eqTxt

tripCost :: TripInfo -> Int
tripCost trip = L.sum $ (\ee -> getData ee price) <$> tripEquipment trip

tripLength :: TripInfo -> Int
tripLength trip = L.sum $ (\ee -> getData ee timeAdded) <$> tripEquipment trip


initTripProgress :: GameData -> T.Text -> [T.Text] -> GameConfigs -> (GameData, TripState)
initTripProgress gd loc eqKeys cfgs = (gd', TripState trip aType (length aType)  [])
    where
        playCfgs = sharkCfgs cfgs
        trip = tripInfo playCfgs loc eqKeys
        (gd', aType) = catchAttempts playCfgs gd trip


catchAttempts :: PlayConfigs -> GameData -> TripInfo -> (GameData, [TripAttempt])
catchAttempts cfgs gd trip = (gd { gameDataSeed = s'}, n)
    where
        months = foldl (\l ee@(Entry k eq) -> replicate (timeAdded eq) ee ++ l) [] $ tripEquipment trip
        (s', n) = catchAttempts' 1 (gameDataSeed gd) [] months
        catchAttempts' _ s n [] = (s, n)
        catchAttempts' i s n (h:tl) =
            let (s', b) = getRandomBoolS s
            in catchAttempts' (i+1) s' (n ++ ((TripAttempt i h) : (if b then [(TripAttempt i h)] else []))) tl

executeTrip :: PlayConfigs -> GameData -> TripInfo -> DataEntry GameEquipment -> (GameData, Maybe SharkFind)
executeTrip cfgs gd trip eq =
    case sM of
        Nothing -> (gd', Nothing)
        Just shark ->  (gd', Just (SharkFind (entryKey (tripDestination trip)) shark (getData eq infoType)))
    where
        loc = entryData (tripDestination trip)
        sharks = Nothing : (Just <$> sharksFound loc)
        (gd', sM) = getRandomElem gd sharks
