
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
mkSharkFind cfgs (GameShark m s l e) = SharkFind m (getEntry (sharks cfgs) s) (getEntry (siteLocations cfgs) l) (getEntry (equipment cfgs) e)

mkSharkFindFromIndex :: PlayConfigs -> GameData -> SharkIndex -> SharkFind
mkSharkFindFromIndex cfgs gd i = mkSharkFind cfgs gsd
    where
        gsd = getShark gd i

mkGameShark :: SharkFind -> GameSharkData
mkGameShark sf = GameShark (findMonth sf) (entryKey (findSpecies sf)) (entryKey (findLocation sf)) (entryKey (findEquipment sf))
