module Shark.Trip
    ( catchAttempts
    , executeTrip
    , tripInfo
    , tripCost
    , tripLength
    , initTripProgress
    ) where

import SaveData
import Configs
import Shark.Types

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List as L

tripInfo :: PlayConfigs -> T.Text -> [T.Text] -> TripInfo
tripInfo cfg loc eqTxt = TripInfo loc $ (\t -> equipment cfg M.! t) <$> eqTxt

tripCost :: TripInfo -> Int
tripCost trip = L.sum $ price <$> tripEquipment trip

tripLength :: TripInfo -> Int
tripLength trip = L.sum $ timeAdded <$> tripEquipment trip


initTripProgress :: GameData -> T.Text -> [T.Text] -> GameConfigs -> (GameData, TripState)
initTripProgress gd loc eqKeys cfgs = (gd', TripState trip aType (length aType)  [])
    where
        playCfgs = sharkCfgs cfgs
        trip = tripInfo playCfgs loc eqKeys
        (gd', aType) = catchAttempts playCfgs gd trip


catchAttempts :: PlayConfigs -> GameData -> TripInfo -> (GameData, [(Int, T.Text, T.Text)])
catchAttempts cfgs gd trip = (gd { gameDataSeed = s'}, n)
    where
        months = foldl (\l eq -> replicate (timeAdded eq) (infoType eq) ++ l) [] $ tripEquipment trip
        (s', n) = catchAttempts' 1 (gameDataSeed gd) [] months
        catchAttempts' _ s n [] = (s, n)
        catchAttempts' i s n (h:tl) =
            let (s', b) = getRandomBoolS s
            in catchAttempts' (i+1) s' (n ++ ((i, h) : (if b then [(i, h)] else []))) tl

executeTrip :: PlayConfigs -> GameData -> TripInfo -> T.Text -> (GameData, Maybe SharkFind)
executeTrip cfgs gd trip infoType =
    case sM of
        Nothing -> (gd', Nothing)
        Just shark ->  (gd', Just (SharkFind (tripDestination trip) shark infoType))
    where
        loc = (siteLocations cfgs) M.! (tripDestination trip)
        sharks = Nothing : (Just <$> sharksFound loc)
        (gd', sM) = getRandomElem gd sharks
