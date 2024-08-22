module Shark.Trip
    ( catchAttempts
    , executeTrip
    ) where

import SaveData
import Configs

import qualified Data.Text as T
import qualified Data.Map.Strict as M

catchAttempts :: PlayConfigs -> GameData -> TripInfo -> (GameData, [T.Text])
catchAttempts cfgs gd trip = (gd { gameDataSeed = s'}, n)
    where
        months = foldl (\l eq -> replicate (timeAdded eq) (infoType eq) ++ l) [] $ tripEquipment trip
        (s', n) = catchAttempts' (gameDataSeed gd) [] months
        catchAttempts' s n [] = (s, n)
        catchAttempts' s n (h:tl) =
            let (s', b) = getRandomBoolS s
            in catchAttempts' s' (n ++ (h : (if b then [h] else []))) tl

executeTrip :: PlayConfigs -> GameData -> TripInfo -> T.Text -> (GameData, Maybe SharkFind)
executeTrip cfgs gd trip infoType =
    case sM of
        Nothing -> (gd', Nothing)
        Just shark ->  (gd', Just (SharkFind (tripDestination trip) shark infoType))
    where
        loc = (siteLocations cfgs) M.! (tripDestination trip)
        sharks = Nothing : (Just <$> sharksFound loc)
        (gd', sM) = getRandomElem gd sharks
