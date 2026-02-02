{-# LANGUAGE OverloadedStrings #-}

module Generate
    ( defaultGameData
    , generateRandomSharks
    , generateShark
    , withFunds
    , withSharks
    , withEquipment
    , withBoat
    , withMonth
    , withRegion
    , sampleSharks
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L
import Data.Word (Word32)
import System.Random.MWC as R
import Data.Vector.Unboxed (fromList)

import GameData.Types (GameData(..), GameSharkData(..), GameDataEquipment(..))
import GameData.Util (addShark)
import GameData.Random (getRandomElem)
import Configs
import Shark.Types

defaultGameData :: GameConfigs -> IO GameData
defaultGameData cfgs = do
    let startCfg = mechanicsStart $ gameMechanics $ sharkCfgs cfgs
        startBoat = startingBoat startCfg
        gEq = GameEquipment startBoat [startBoat] (startingEquipment startCfg)
        startMoney = startingFunds startCfg
    seed <- createSystemRandom >>= save
    return $ GameData
        { gameDataSaveFile = "test-save.save"
        , gameDataSeed = seed
        , gameDataFoundationNames = []
        , gameDataDonorList = []
        , gameDataFunds = startMoney
        , gameDataMonth = 0
        , gameDataSharkIndex = 0
        , gameDataSharks = M.empty
        , gameDataFoundSharks = M.empty
        , gameDataResearchComplete = M.empty
        , gameDataEquipment = gEq
        , gameCurrentRegion = startingRegion startCfg
        }

defaultGameDataWithSeed :: GameConfigs -> Seed -> GameData
defaultGameDataWithSeed cfgs seed =
    let startCfg = mechanicsStart $ gameMechanics $ sharkCfgs cfgs
        startBoat = startingBoat startCfg
        gEq = GameEquipment startBoat [startBoat] (startingEquipment startCfg)
        startMoney = startingFunds startCfg
    in GameData
        { gameDataSaveFile = "test-save.save"
        , gameDataSeed = seed
        , gameDataFoundationNames = []
        , gameDataDonorList = []
        , gameDataFunds = startMoney
        , gameDataMonth = 0
        , gameDataSharkIndex = 0
        , gameDataSharks = M.empty
        , gameDataFoundSharks = M.empty
        , gameDataResearchComplete = M.empty
        , gameDataEquipment = gEq
        , gameCurrentRegion = startingRegion startCfg
        }

seedFromInt :: Int -> Seed
seedFromInt n = toSeed $ fromList [fromIntegral n, 0, 0, 0 :: Word32]

generateRandomSharks :: GameConfigs -> Int -> Int -> IO GameData
generateRandomSharks cfgs seedInt count = do
    let seed = seedFromInt seedInt
        gd = defaultGameDataWithSeed cfgs seed
        playCfgs = sharkCfgs cfgs
    return $ generateSharksLoop playCfgs gd count

generateSharksLoop :: PlayConfigs -> GameData -> Int -> GameData
generateSharksLoop _ gd 0 = gd
generateSharksLoop cfgs gd n =
    let (gd', shark) = generateShark cfgs gd
        gd'' = addShark gd' shark
    in generateSharksLoop cfgs gd'' (n - 1)

generateShark :: PlayConfigs -> GameData -> (GameData, GameSharkData)
generateShark cfgs gd = (gd4, GameShark month species region location equip)
    where
        speciesList = M.keys (sharks cfgs)
        regionList = M.keys (regions cfgs)
        equipList = M.keys (equipment cfgs)
        (gd1, species) = getRandomElem gd speciesList
        (gd2, region) = getRandomElem gd1 regionList
        regionData = regions cfgs M.! region
        locationList = M.keys (siteLocations regionData)
        (gd3, location) = getRandomElem gd2 locationList
        (gd4, equip) = getRandomElem gd3 equipList
        month = gameDataMonth gd

withFunds :: GameData -> Int -> GameData
withFunds gd funds = gd { gameDataFunds = funds }

withSharks :: GameData -> [GameSharkData] -> GameData
withSharks = L.foldl' addShark

withEquipment :: GameData -> [T.Text] -> GameData
withEquipment gd eqs = gd { gameDataEquipment = eq' }
    where
        eq' = (gameDataEquipment gd) { gameOwnedEquipment = L.nub $ eqs ++ gameOwnedEquipment (gameDataEquipment gd) }

withBoat :: GameData -> T.Text -> GameData
withBoat gd boat = gd { gameDataEquipment = eq' }
    where
        eq' = (gameDataEquipment gd)
            { gameActiveBoat = boat
            , gameOwnedBoats = L.nub $ boat : gameOwnedBoats (gameDataEquipment gd)
            }

withMonth :: GameData -> Int -> GameData
withMonth gd m = gd { gameDataMonth = m }

withRegion :: GameData -> T.Text -> GameData
withRegion gd r = gd { gameCurrentRegion = r }

sampleSharks :: PlayConfigs -> [GameSharkData]
sampleSharks cfgs =
    let species = head $ M.keys (sharks cfgs)
        region = head $ M.keys (regions cfgs)
        regionData = regions cfgs M.! region
        location = head $ M.keys (siteLocations regionData)
        equip = head $ M.keys (equipment cfgs)
    in [ GameShark 0 species region location equip
       , GameShark 1 species region location equip
       , GameShark 2 species region location equip
       ]