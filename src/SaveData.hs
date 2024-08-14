{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module SaveData
    ( GameData(..)
    , startNewGame
    , getRandomPercent
    , loadFromFile
    , saveToFile
    , TripInfo(..)
    , tripInfo
    , tripCost
    ) where

import System.IO ()
import Paths_shark_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Word
import System.Random.MWC as R
import Control.Monad
import Control.Monad.ST.Lazy (runST)
import Data.Vector.Unboxed
import qualified Data.List as L

import Configs
import Env.Files (getGameDirectory)

getRandomPercent :: GameData -> (GameData, Int)
getRandomPercent gd = runST $ do
        gen <- restore s
        p <- uniformR (0, 100) gen
        s' <- save gen
        return $ (gd { gameDataSeed = s' }, p)
    where
        s = gameDataSeed gd

data TripInfo = TripInfo
    { tripDestination :: GameLocation
    , tripEquipment :: [GameEquipment]
    }

tripInfo :: PlayConfigs -> GameLocation -> [T.Text] -> TripInfo
tripInfo cfg loc eqTxt = TripInfo loc $ (\t -> equipment cfg M.! t) <$> eqTxt

tripCost :: TripInfo -> Int
tripCost trip = L.sum $ price <$> tripEquipment trip

data GameData = GameData
    { gameDataSaveFile :: String
    , gameDataSeed :: Seed
    , gameDataFunds :: Int
    , gameDataMonth :: Int
    }


data GameSaveData = GameSaveData
    { saveSeed :: Vector Word32
    , saveFunds :: Int
    , saveMonth :: Int
    } deriving (Show, Eq, Generic)


instance FromJSON GameSaveData
instance ToJSON GameSaveData

startNewGame :: IO GameData
startNewGame = do
    g <- R.create
    name <- uniform g :: IO Int
    let dir = "data/saves"
    path <- getGameDirectory dir
    let fn = path L.++ "/file" L.++ (show name) L.++ ".save"
    putStrLn fn
    s <- save g
    return $ GameData fn s 0 0


convertSave :: String -> GameSaveData -> GameData
convertSave fn gsd = GameData fn seed (saveFunds gsd) (saveMonth gsd)
    where
        seed = toSeed $ saveSeed gsd

convertBack :: GameData -> (FilePath, GameSaveData)
convertBack gd = (gameDataSaveFile gd, GameSaveData seedV (gameDataFunds gd) (gameDataMonth gd))
    where
        seedV = fromSeed $ gameDataSeed gd

loadFromFile :: FilePath -> IO GameData
loadFromFile fp = do
    gsdM <- eitherDecodeFileStrict fp
    case gsdM of
        Left err -> error ("Failed to open save file " L.++ (show err))
        Right gsd -> return $ convertSave fp gsd

saveToFile :: GameData -> IO ()
saveToFile gd = do
    putStrLn $ "saving file to: " L.++ fn
    encodeFile fn gsd
    where
        (fn, gsd) = convertBack gd

class Monad m => SaveData m where
    saveData :: m ()
    loadData :: m GameData
