{-# LANGUAGE OverloadedStrings #-}

module Shark.Review
    ( getInfoCounts
    , getFinds
    , getKnownResearch
    , getResearchRequirements
    , getSeenLocations
    , canCompleteResearch
    , completeResearch
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L

import Shark.Types
import Shark.Util
import SaveData
import Util

getFinds :: PlayConfigs -> GameData -> DataEntryT SharkInfo -> [SharkFind]
getFinds cfgs gd se =
    case M.lookup sharkKey gsd of
        Nothing -> []
        Just sdi -> mkSharkFindFromIndex cfgs gd <$> sdi
    where
        sharkKey = entryKey se
        gsd = gameDataFoundSharks gd

getSeenLocations :: PlayConfigs -> GameData -> DataEntryT SharkInfo -> M.Map T.Text [T.Text]
getSeenLocations cfgs gd se = foldr addLocation M.empty $ L.nub $ findLocation <$> getFinds cfgs gd se
    where
        addLocation lE acc = 
            let region = getData lE (regionShowText . locationRegion)
                site = getData lE (showText . locationSite)
            in M.insertWith (++) region [site] acc

getInfoCounts :: [SharkFind] -> M.Map T.Text Int
getInfoCounts = getInfoCounts' M.empty
    where
        getInfoCounts' m [] = m
        getInfoCounts' m (h:tl) =
            let eq = findEquipment h
                info = getData eq equipInfoType
            in getInfoCounts' (M.insertWith (+) info 1 m) tl

getKnownSharks :: PlayConfigs -> GameData -> [DataEntryT SharkInfo]
getKnownSharks cfgs gd = getEntry (sharks cfgs) <$> M.keys (gameDataFoundSharks gd)

getResearchSharks :: ResearchData -> [T.Text]
getResearchSharks rr = M.keys $ researchReqs rr

getKnownResearch :: PlayConfigs -> GameData -> [DataEntryT ResearchData]
getKnownResearch cfgs gd = foldl getKnownResearch' [] researchEntries
    where
        researchEntries = uncurry Entry <$> M.toList (research cfgs)
        kSharks = S.fromList $ entryKey <$> getKnownSharks cfgs gd
        getKnownResearch' l rE = if shouldShowResearch cfgs gd rE then l ++ [rE] else l


shouldShowResearch :: PlayConfigs -> GameData -> DataEntryT ResearchData -> Bool
shouldShowResearch cfgs gd re = rSharks `S.isSubsetOf` kSharks && depends `S.isSubsetOf` completed
    where
        kSharks = S.fromList $ entryKey <$> getKnownSharks cfgs gd
        rSharks = S.fromList $ getResearchSharks (entryData re)
        completed = S.fromList $ M.keys $ gameDataResearchComplete gd
        depends = S.fromList $ researchDepends (entryData re)

type SharkReq = (T.Text, [SharkIndex], Int)
type ResearchReqData = (T.Text, [SharkReq])

getResearchRequirements :: PlayConfigs -> GameData -> DataEntryT ResearchData -> [ResearchReqData]
getResearchRequirements cfgs gd re = researchReq <$> bySharks
    where
        bySharks = M.toList $ researchReqs (entryData re)
        researchReq (sk, reqs) = (getName sk, getResearchRequirement cfgs gd sk <$> reqs)
        getName sk = sharkName $ sharks cfgs M.! sk

getResearchRequirement :: PlayConfigs -> GameData -> T.Text -> ResearchReq -> SharkReq
getResearchRequirement cfgs gd sk req = (iType, sInfos, reqCount req)
    where
        sds = gameDataFoundSharks gd M.! sk
        iType = dataType req
        sInfos = fst <$> L.filter (\(i, sd) -> getSharkInfo cfgs sd == iType) ((\sdi -> (sdi, getShark gd sdi)) <$> sds)

getSharkInfo :: PlayConfigs -> GameSharkData -> T.Text
getSharkInfo cfgs sd = equipInfoType $ equipment cfgs M.! gameSharkEquipment sd

canCompleteResearch :: [ResearchReqData] -> Bool
canCompleteResearch = all canComplete
    where
        canComplete (_, reqs) = all canComplete' reqs
        canComplete' (_, sds, c) = length sds >= c

completeResearch :: PlayConfigs -> GameData -> DataEntryT ResearchData -> [ResearchReqData] -> GameData
completeResearch cfgs gd re reqs = gd { gameDataResearchComplete = M.insert (entryKey re) rci (gameDataResearchComplete gd)
                                      , gameDataFunds = gameDataFunds gd + grant }
    where
        sharksComp = concatMap (\(_, entries) -> concatMap (\(_, sis, _) -> sis) entries) reqs
        rci = ResearchCompleteInfo (gameDataMonth gd) sharksComp
        grant = researchGrant (entryData re)

