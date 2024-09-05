{-# LANGUAGE OverloadedStrings #-}

module Shark.Trip
    ( catchAttempts
    , executeTrip
    , tripCost
    , tripInfo
    , tripLength
    , initTripProgress
    , mkSharkFind
    , mkGameShark
    , monthToText
    ) where

import SaveData
import Configs
import Shark.Types
import Util

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List as L

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

tripInfo :: PlayConfigs -> T.Text -> [T.Text] -> TripInfo
tripInfo cfg loc eqTxt = TripInfo locE eqEs
    where
        locE = getEntry (siteLocations cfg) loc
        eqEs = (\k -> getEntry (equipment cfg) k) <$> eqTxt

tripCost :: TripInfo -> Int
tripCost trip = L.sum $ (\ee -> getData ee price) <$> tripEquipment trip

tripLength :: TripInfo -> Int
tripLength trip = L.sum $ (\ee -> getData ee timeAdded) <$> tripEquipment trip


initTripProgress :: GameData -> T.Text -> [T.Text] -> GameConfigs -> (GameData, TripState)
initTripProgress gd loc eqKeys cfgs = (gd', TripState trip aType (length aType)  [])
    where
        playCfgs = sharkCfgs cfgs
        trip = tripInfo playCfgs loc eqKeys
        (gd', aType) = catchAttempts playCfgs gd trip


catchAttempts :: PlayConfigs -> GameData -> TripInfo -> (GameData, [TripAttempt])
catchAttempts cfgs gd trip = (gd { gameDataSeed = s'}, n)
    where
        months = foldl (\l ee@(Entry k eq) -> replicate (timeAdded eq) ee ++ l) [] $ tripEquipment trip
        (s', n) = catchAttempts' 1 (gameDataSeed gd) [] months
        catchAttempts' _ s n [] = (s, n)
        catchAttempts' i s n (h:tl) =
            let (s', b) = getRandomBoolS s
            in catchAttempts' (i+1) s' (n ++ ((TripAttempt i h) : (if b then [(TripAttempt i h)] else []))) tl

executeTrip :: PlayConfigs -> GameData -> TripInfo -> TripAttempt -> (GameData, Maybe SharkFind)
executeTrip cfgs gd trip (TripAttempt mnth eq) =
    case sM of
        Nothing -> (gd', Nothing)
        Just shark ->  (gd', Just (SharkFind (curMnth + mnth - 1) (getEntry (sharks cfgs) shark) (tripDestination trip) eq))
    where
        curMnth = gameDataMonth gd
        loc = entryData (tripDestination trip)
        sharksAround = Nothing : (Just <$> sharksFound loc)
        (gd', sM) = getRandomElem gd sharksAround

mkSharkFind :: PlayConfigs -> [GameSharkData] -> [SharkFind]
mkSharkFind cfgs [] = []
mkSharkFind cfgs (h:tl) = convert h : mkSharkFind cfgs tl
    where
        convert (GameShark m s l e) = SharkFind m (getEntry (sharks cfgs) s) (getEntry (siteLocations cfgs) l) (getEntry (equipment cfgs) e)

mkGameShark :: SharkFind -> GameSharkData
mkGameShark sf = GameShark (findMonth sf) (entryKey (findSpecies sf)) (entryKey (findLocation sf)) (entryKey (findEquipment sf))
