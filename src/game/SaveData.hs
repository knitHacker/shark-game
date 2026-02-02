{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module SaveData
    ( GameData(..)
    , GameSharkData(..)
    , ResearchCompleteInfo(..)
    , GameDataEquipment(..)
    , SharkIndex
    , startNewGame
    , getRandomPercent
    , getRandomPercentS
    , getRandomRange
    , getRandomBool
    , getRandomBoolS
    , getRandomElem
    , loadFromFile
    , saveToFile
    , addShark
    , getShark
    ) where

import System.IO ()
import Paths_shark_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath

import Data.Word
import System.Random.MWC as R
import Control.Monad
import Control.Monad.ST.Lazy (runST)
import Data.Vector.Unboxed
import qualified Data.List as L

import Configs
import Env.Files (getGameDirectory, getLocalGamePath)
import Shark.Types

getRandomPercent :: GameData -> (GameData, Int)
getRandomPercent gd = (gd { gameDataSeed = s' }, p)
    where
        s = gameDataSeed gd
        (s', p) = getRandomPercentS s

getRandomRange :: GameData -> Int -> Int -> (GameData, Int)
getRandomRange gd minVal maxVal = (gd { gameDataSeed = s' }, p)
    where
        s = gameDataSeed gd
        (s', p) = getRandomRangeS s minVal maxVal

getRandomBool :: GameData -> (GameData, Bool)
getRandomBool gd = (gd { gameDataSeed = s' }, p)
    where
        s = gameDataSeed gd
        (s', p) = getRandomBoolS s

getRandomPercentS :: Seed -> (Seed, Int)
getRandomPercentS s = runST $ do
        gen <- restore s
        p <- uniformR (0, 100) gen
        s' <- save gen
        return (s', p)

getRandomRangeS :: Seed -> Int -> Int -> (Seed, Int)
getRandomRangeS s minVal maxVal = runST $ do
        gen <- restore s
        p <- uniformR (minVal, maxVal) gen
        s' <- save gen
        return (s', p)

getRandomBoolS :: Seed -> (Seed, Bool)
getRandomBoolS s = runST $ do
        gen <- restore s
        p <- uniformM gen
        s' <- save gen
        return (s', p)


getRandomElem :: GameData -> [a] -> (GameData, a)
getRandomElem gd ls = runST $ do
        gen <- restore s
        p <- uniformR (0, len - 1) gen
        s' <- save gen
        return (gd { gameDataSeed = s' }, ls !! p)
    where
        s = gameDataSeed gd
        len = L.length ls

type SharkIndex = Int

data GameDataEquipment = GameEquipment
    { gameActiveBoat :: T.Text
    , gameOwnedBoats :: [T.Text]
    , gameOwnedEquipment :: [T.Text]
    } deriving (Show, Eq, Generic)

instance FromJSON GameDataEquipment
instance ToJSON GameDataEquipment

data GameSharkData = GameShark
    { gameSharkMonth :: Int
    , gameSharkSpecies :: T.Text
    , gameSharkRegion :: T.Text
    , gameSharkLocation :: T.Text
    , gameSharkEquipment :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON GameSharkData
instance ToJSON GameSharkData

data ResearchCompleteInfo = ResearchCompleteInfo
    { researchCompleteMonth :: Int
    , researchCompleteSharks :: [SharkIndex]
    } deriving (Show, Eq, Generic)

instance FromJSON ResearchCompleteInfo
instance ToJSON ResearchCompleteInfo


data GameData = GameData
    { gameDataSaveFile :: String
    , gameDataSeed :: Seed
    , gameDataFoundationNames :: [T.Text]
    , gameDataDonorList :: [T.Text]
    , gameDataFunds :: Int
    , gameDataMonth :: Int
    , gameDataSharkIndex :: SharkIndex
    , gameDataSharks :: M.Map SharkIndex GameSharkData
    , gameDataFoundSharks :: M.Map T.Text [SharkIndex]
    , gameDataResearchComplete :: M.Map T.Text ResearchCompleteInfo
    , gameDataEquipment :: GameDataEquipment
    , gameCurrentRegion :: T.Text
    } deriving (Show, Eq)


data GameSaveData = GameSaveData
    { saveSeed :: Vector Word32
    , saveFoundationNames :: [T.Text]
    , saveDonorList :: [T.Text]
    , saveFunds :: Int
    , saveMonth :: Int
    , saveSharkIndex :: SharkIndex
    , saveSharks :: M.Map SharkIndex GameSharkData
    , saveResearchComplete :: M.Map T.Text ResearchCompleteInfo
    , saveDataEquipment :: GameDataEquipment
    , saveCurrentRegion :: T.Text
    } deriving (Show, Eq, Generic)


instance FromJSON GameSaveData
instance ToJSON GameSaveData

startNewGame :: GameConfigs -> IO GameData
startNewGame cfgs = do
    let startCfg = mechanicsStart $ gameMechanics $ sharkCfgs cfgs
    g <- createSystemRandom
    name <- uniform g :: IO Int
    let nameStr = "shark-" L.++ show (abs name) L.++ ".save"
    let dir = "data" </> "saves" </> nameStr
    path <- getLocalGamePath dir
    -- putStrLn path
    s <- save g
    let startBoat = startingBoat startCfg
        gEq = GameEquipment startBoat [startBoat] (startingEquipment startCfg)
        startMoney = startingFunds startCfg
        gData = GameData path s [] [] startMoney 0 0 M.empty M.empty M.empty gEq $ startingRegion startCfg
    createDirectoryIfMissing True (takeDirectory path)
    saveToFile gData
    return gData

sortSharks :: M.Map SharkIndex GameSharkData -> M.Map T.Text [SharkIndex]
sortSharks = M.foldlWithKey' (\m i sd -> M.insertWith (L.++) (gameSharkSpecies sd) [i] m) M.empty

convertSave :: String -> GameSaveData -> GameData
convertSave fn gsd = GameData fn seed (saveFoundationNames gsd) (saveDonorList gsd)
                              (saveFunds gsd) (saveMonth gsd) (saveSharkIndex gsd) sSharks
                              (sortSharks sSharks) (saveResearchComplete gsd) (saveDataEquipment gsd)
                              (saveCurrentRegion gsd)
    where
        seed = toSeed $ saveSeed gsd
        sSharks = saveSharks gsd

convertBack :: GameData -> (FilePath, GameSaveData)
convertBack gd = ( gameDataSaveFile gd
                 , GameSaveData seedV (gameDataFoundationNames gd) (gameDataDonorList gd) (gameDataFunds gd) (gameDataMonth gd)
                                      (gameDataSharkIndex gd) (gameDataSharks gd) (gameDataResearchComplete gd)
                                      (gameDataEquipment gd) (gameCurrentRegion gd)
                 )
    where
        seedV = fromSeed $ gameDataSeed gd

loadFromFile :: FilePath -> IO (Either T.Text GameData)
loadFromFile fp = do
    gsdM <- eitherDecodeFileStrict fp
    case gsdM of
        Left err -> return $ Left $ T.pack ("Failed to open save file " L.++ show err)
        Right gsd -> return $ Right $ convertSave fp gsd

saveToFile :: GameData -> IO ()
saveToFile gd = do
    -- putStrLn $ "saving file to: " L.++ fn
    encodeFile fn gsd
    where
        (fn, gsd) = convertBack gd

addShark :: GameData -> GameSharkData -> GameData
addShark gd gsd = gd { gameDataSharkIndex = i + 1
                     , gameDataSharks = M.insert i gsd (gameDataSharks gd)
                     , gameDataFoundSharks = M.insertWith (L.++) (gameSharkSpecies gsd) [i] (gameDataFoundSharks gd)
                     }
    where
        i = gameDataSharkIndex gd

getShark :: GameData -> SharkIndex -> GameSharkData
getShark gd i = gameDataSharks gd M.! i

class Monad m => SaveData m where
    saveData :: m ()
    loadData :: m GameData
