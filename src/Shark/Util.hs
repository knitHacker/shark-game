
module Shark.Util
    ( mkSharkFind
    , mkGameShark
    ) where

import Shark.Types
import SaveData
import Util

mkSharkFind :: PlayConfigs -> [GameSharkData] -> [SharkFind]
mkSharkFind cfgs [] = []
mkSharkFind cfgs (h:tl) = convert h : mkSharkFind cfgs tl
    where
        convert (GameShark m s l e) = SharkFind m (getEntry (sharks cfgs) s) (getEntry (siteLocations cfgs) l) (getEntry (equipment cfgs) e)

mkGameShark :: SharkFind -> GameSharkData
mkGameShark sf = GameShark (findMonth sf) (entryKey (findSpecies sf)) (entryKey (findLocation sf)) (entryKey (findEquipment sf))
