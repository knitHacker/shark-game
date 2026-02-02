
module Shark.Store
    ( buyEquipment
    , buyBoat
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L

import SaveData
import Shark.Types

buyEquipment :: GameData -> T.Text -> Int -> GameData
buyEquipment gd eqKey price = gd { gameDataFunds = funds', gameDataEquipment = eq' }
    where
        funds' = gameDataFunds gd - price
        eq' = (gameDataEquipment gd) { gameOwnedEquipment = L.nub $ eqKey : gameOwnedEquipment (gameDataEquipment gd) }

buyBoat :: GameData -> T.Text -> Int -> GameData
buyBoat gd boatKey price = gd { gameDataFunds = funds', gameDataEquipment = eq' }
    where
        funds' = gameDataFunds gd - price
        eq' = (gameDataEquipment gd) { gameActiveBoat = boatKey, gameOwnedBoats = L.nub $ boatKey : gameOwnedBoats (gameDataEquipment gd) }