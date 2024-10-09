
module Shark.Review
    ( getInfoCounts
    , getFinds
    , getKnownResearch
    , getResearchRequirements
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L

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

getKnownSharks :: PlayConfigs -> GameData -> [DataEntry SharkInfo]
getKnownSharks cfgs gd = getEntry (sharks cfgs) <$> M.keys (gameDataFoundSharks gd)

getResearchSharks :: ResearchData -> [T.Text]
getResearchSharks rr = M.keys $ researchReqs rr

getKnownResearch :: PlayConfigs -> GameData -> [DataEntry ResearchData]
getKnownResearch cfgs gd = foldl getKnownResearch' [] researchEntries
    where
        researchEntries = uncurry Entry <$> M.toList (research cfgs)
        kSharks = S.fromList $ entryKey <$> getKnownSharks cfgs gd
        getKnownResearch' l rE = if shouldShowResearch cfgs gd rE then l ++ [rE] else l


shouldShowResearch :: PlayConfigs -> GameData -> DataEntry ResearchData -> Bool
shouldShowResearch cfgs gd re = rSharks `S.isSubsetOf` kSharks && depends `S.isSubsetOf` completed
    where
        kSharks = S.fromList $ entryKey <$> getKnownSharks cfgs gd
        rSharks = S.fromList $ getResearchSharks (entryData re)
        completed = S.fromList $ M.keys $ gameDataResearchComplete gd
        depends = S.fromList $ researchDepends (entryData re)

getResearchRequirements :: PlayConfigs -> GameData -> DataEntry ResearchData -> [(T.Text, [(T.Text, Int, Int)])]
getResearchRequirements cfgs gd re = researchReq <$> bySharks
    where
        bySharks = M.toList $ researchReqs (entryData re)
        researchReq (sk, reqs) = (getName sk, getResearchRequirement cfgs gd sk <$> reqs)
        getName sk = sharkName $ sharks cfgs M.! sk

getResearchRequirement :: PlayConfigs -> GameData -> T.Text -> ResearchReq -> (T.Text, Int, Int)
getResearchRequirement cfgs gd sk req = (iType, sInfos, reqCount req)
    where
        sds = gameDataFoundSharks gd M.! sk
        iType = dataType req
        sInfos = length $ L.filter (\sd -> getSharkInfo cfgs sd == iType) sds

getSharkInfo :: PlayConfigs -> GameSharkData -> T.Text
getSharkInfo cfgs sd = infoType $ equipment cfgs M.! gameSharkEquipment sd

--completeResearch :: PlayConfigs -> GameData -> DataEntry ResearchData -> GameData
--completeResearch cfgs gd re = gd { gameDataResearchComplete = M.insert (entryKey re) rci (gameDataResearchComplete gd) }
--    where
--        rci = ResearchCompleteInfo (gameDataMonth gd) []

