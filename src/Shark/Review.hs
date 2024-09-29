
module Shark.Review
    ( getInfoCounts
    , getFinds
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Shark.Types
import Shark.Util
import SaveData
import Util

getFinds :: PlayConfigs -> GameData -> DataEntry SharkInfo -> [SharkFind]
getFinds cfgs gd se =
    case M.lookup sharkKey gsd of
        Nothing -> []
        Just sd -> mkSharkFind cfgs sd
    where
        sharkKey = entryKey se
        gsd = gameDataFoundSharks gd


getInfoCounts :: [SharkFind] -> M.Map T.Text Int
getInfoCounts = getInfoCounts' M.empty
    where
        getInfoCounts' m [] = m
        getInfoCounts' m (h:tl) =
            let eq = findEquipment h
                info = getData eq infoType
            in getInfoCounts' (M.insertWith (+) info 1 m) tl

