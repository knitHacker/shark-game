
module Shark.Store
    ( buyEquipment
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import SaveData
import Shark.Types

buyEquipment :: GameData -> T.Text -> Int -> GameData
buyEquipment gd eqKey price = gd { gameDataFunds = funds', gameDataEquipment = eq' }
    where
        funds' = gameDataFunds gd - price
        eq' = (gameDataEquipment gd) { gameOwnedEquipment = eqKey : gameOwnedEquipment (gameDataEquipment gd) }
