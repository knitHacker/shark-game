module Shark.Trip
    ( catchAttempts
    , executeTrip
    ) where

import SaveData
import Configs

catchAttempts :: PlayConfigs -> GameData -> TripInfo -> (GameData, Int)
catchAttempts cfgs gd trip = (gd { gameDataSeed = s'}, n)
    where
        months = tripLength trip
        (s', n) = catchAttempts' (gameDataSeed gd) 0 months
        catchAttempts' s n 0 = (s, n)
        catchAttempts' s n m =
            let (s', b) = getRandomBoolS s
            in catchAttempts' s' (n + 1 + if b then 1 else 0) (m - 1)

executeTrip :: PlayConfigs -> GameData -> TripInfo -> Maybe SharkFind
executeTrip cfgs gd trip = Nothing
