
module Shark.Util
    ( mkSharkFinds
    , mkSharkFind
    , mkSharkFindFromIndex
    , mkGameShark
    ) where

import Shark.Types
import SaveData
import Util

mkSharkFinds :: PlayConfigs -> [GameSharkData] -> [SharkFind]
mkSharkFinds cfgs gsd = mkSharkFind cfgs <$> gsd

mkSharkFind :: PlayConfigs -> GameSharkData -> SharkFind
mkSharkFind cfgs (GameShark m s r l e) = SharkFind m (getEntry (sharks cfgs) s) loc (getEntry (equipment cfgs) e)
    where
        loc = getNestedEntry (regions cfgs) siteLocations (\ rD lD -> FullLocation (regionInfo rD) lD) r l

mkSharkFindFromIndex :: PlayConfigs -> GameData -> SharkIndex -> SharkFind
mkSharkFindFromIndex cfgs gd i = mkSharkFind cfgs gsd
    where
        gsd = getShark gd i

mkGameShark :: SharkFind -> GameSharkData
mkGameShark sf = GameShark (findMonth sf) (entryKey (findSpecies sf)) r l (entryKey (findEquipment sf))
    where
        (r, l) = entryKey (findLocation sf)
