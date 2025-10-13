{-# LANGUAGE OverloadedStrings #-}

module Shark.Trip
    ( catchAttempts
    , executeTrip
    , tripCost
    , tripInfo
    , tripLength
    , initTripProgress
    , monthToText
    ) where

import SaveData
import Configs
import Shark.Types
import Util

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (isJust)

import Debug.Trace

monthToText :: Int -> T.Text
monthToText 0 = "0 months"
monthToText mnths
    | year == 0 = monthTxt month
    | month == 0 = yearTxt year
    | otherwise = T.concat [yearTxt year, ", ", monthTxt month]
    where
        year = div mnths 12
        month = mod mnths 12
        yearTxt 0 = ""
        yearTxt 1 = "1 year"
        yearTxt n = T.pack (show n ++ " years")
        monthTxt 0 = ""
        monthTxt 1 = "1 month"
        monthTxt n = T.pack (show n ++ " months")

tripInfo :: PlayConfigs -> T.Text -> T.Text -> [T.Text] -> TripInfo
tripInfo cfg loc boatName eqTxt = TripInfo locE eqEs boat
    where
        locE = getEntry (siteLocations cfg) loc
        eqEs = getEntry (equipment cfg) <$> eqTxt
        boat = getEntry (boats cfg) boatName

tripCost :: TripInfo -> Int
tripCost trip = tripLength trip * tripFuelCost
    where
        tripFuelCost = boatFuelCost $ entryData (tripBoat trip)

tripLength :: TripInfo -> Int
tripLength trip = L.sum $ flip getData equipTimeAdded <$> tripEquipment trip


initTripProgress :: GameData -> T.Text -> T.Text -> [T.Text] -> GameConfigs -> (GameData, TripState)
initTripProgress gd loc boatName eqKeys cfgs = (gd', TripState trip aType (length aType)  [])
    where
        playCfgs = sharkCfgs cfgs
        trip = tripInfo playCfgs loc boatName eqKeys
        (gd', aType) = catchAttempts playCfgs gd trip


catchAttempts :: PlayConfigs -> GameData -> TripInfo -> (GameData, [TripAttempt])
catchAttempts cfgs gd trip = (gd { gameDataSeed = s'}, n)
    where
        mechs = mechanicsEncounter $ gameMechanics cfgs
        months = foldl (\l ee@(Entry k eq) -> replicate (equipTimeAdded eq) ee ++ l) [] $ tripEquipment trip
        (s', n) = catchAttempts' 1 (gameDataSeed gd) [] months
        catchAttempts' _ s n [] = (s, n)
        catchAttempts' i s n (h:tl) =
            let (s', per) = getRandomPercentS s
                base = replicate (baseAttempts mechs) (TripAttempt i h)
                bonusAttempt = bonusAttemptChancePercent mechs > per
                (nextMonth, equips) = if bonusAttempt then (i+1, tl) else (i, h:tl)
            in catchAttempts' nextMonth s' (n ++ base ++ [TripAttempt i h | bonusAttempt]) equips

executeTrip :: PlayConfigs -> GameData -> TripInfo -> TripAttempt -> (GameData, Maybe SharkFind)
executeTrip cfgs gd trip (TripAttempt mnth eq) =
    case sM of
        Nothing -> (gd', Nothing)
        Just shark ->  (gd', Just (SharkFind (curMnth + mnth - 1) (getEntry (sharks cfgs) shark) (tripDestination trip) eq))
    where
        eqChance = getData eq equipEffectiveness
        curMnth = gameDataMonth gd
        loc = entryData (tripDestination trip)
        (gd', caughtP) = getRandomPercent gd
        caughtAnything = eqChance > caughtP
        (_, sharkChances) = foldl (\(p, ls) (s, c) -> (p + c, ls ++ [(s, p+c)])) (0, []) (sharksFound loc)
        (gd'', sharkChoice) = getRandomPercent gd'
        sharkMatch m (s, p)
            | isJust m = m
            | sharkChoice < p = Just s
            | otherwise = Nothing
        sharkM = foldl sharkMatch Nothing sharkChances
        sM = if not caughtAnything || null sharkChances then Nothing else sharkM
