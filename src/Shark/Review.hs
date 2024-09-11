
module Shark.Review
    ( getInfoTypeCount
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Shark.Types
import SaveData

getFinds :: PlayConfigs -> GameData -> DataEntry SharkInfo -> [SharkFind]
getFinds cfgs gd se = 


getInfoCounts :: [SharkFind] -> M.Map T.Text Int
getInfoCounts sd =
    where
        getInfoCounts' m [] = m
        getInfoCounts' m (h:tl) = 

getInfoTypeCount :: [SharkFind] -> T.Text -> Int
getInfoTypeCount [] _ = 0
getInfoTypeCount (h:tl) it
    | infoType eq == it = 1 + getInfoTypeCount tl it
    | otherwise = getInfoTypeCount tl it
    where
        eq = gameSharkEquipment h
