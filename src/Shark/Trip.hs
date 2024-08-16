module Shark.Trip
    (
    ) where

import SaveData
import Configs

catchAttempts :: PlayConfigs -> GameData -> TripInfo -> Int
catchAttempts cfgs gd trip = 0

executeTrip :: PlayConfigs -> GameData -> TripInfo -> Maybe SharkFind
executeTrip cfgs gd trip = Nothing
