{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module SaveData
    ( GameData(..)
    , startNewGame
    , getRandomPercent
    , getRandomPercentS
    , getRandomBool
    , getRandomBoolS
    , getRandomElem
    , loadFromFile
    , saveToFile
    ) where

import System.IO ()
import Paths_shark_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Word
import System.Random.MWC as R
import Control.Monad
import Control.Monad.ST.Lazy (runST)
import Data.Vector.Unboxed
import qualified Data.List as L

import Configs
import Env.Files (getGameDirectory)
import Shark.Types

getRandomPercent :: GameData -> (GameData, Int)
getRandomPercent gd = (gd { gameDataSeed = s' }, p)
    where
        s = gameDataSeed gd
        (s', p) = getRandomPercentS s

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
        return $ (s', p)

getRandomBoolS :: Seed -> (Seed, Bool)
getRandomBoolS s = runST $ do
        gen <- restore s
        p <- uniformM gen
        s' <- save gen
        return $ (s', p)


getRandomElem :: GameData -> [a] -> (GameData, a)
getRandomElem gd ls = runST $ do
        gen <- restore s
        p <- uniformR (0, len - 1) gen
        s' <- save gen
        return $ (gd { gameDataSeed = s' }, ls !! p)
    where
        s = gameDataSeed gd
        len = L.length ls

data GameData = GameData
    { gameDataSaveFile :: String
    , gameDataSeed :: Seed
    , gameDataFunds :: Int
    , gameDataMonth :: Int
    , gameDataFoundSharks :: M.Map T.Text [SharkFind]
    }


data GameSaveData = GameSaveData
    { saveSeed :: Vector Word32
    , saveFunds :: Int
    , saveMonth :: Int
    , saveFoundSharks :: M.Map T.Text [SharkFind]
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
    return $ GameData fn s 0 0 M.empty


convertSave :: String -> GameSaveData -> GameData
convertSave fn gsd = GameData fn seed (saveFunds gsd) (saveMonth gsd) (saveFoundSharks gsd)
    where
        seed = toSeed $ saveSeed gsd

convertBack :: GameData -> (FilePath, GameSaveData)
convertBack gd = (gameDataSaveFile gd, GameSaveData seedV (gameDataFunds gd) (gameDataMonth gd) (gameDataFoundSharks gd))
    where
        seedV = fromSeed $ gameDataSeed gd

loadFromFile :: FilePath -> IO (Either T.Text GameData)
loadFromFile fp = do
    gsdM <- eitherDecodeFileStrict fp
    case gsdM of
        Left err -> return $ Left $ T.pack ("Failed to open save file " L.++ (show err))
        Right gsd -> return $ Right $ convertSave fp gsd

saveToFile :: GameData -> IO ()
saveToFile gd = do
    putStrLn $ "saving file to: " L.++ fn
    encodeFile fn gsd
    where
        (fn, gsd) = convertBack gd

class Monad m => SaveData m where
    saveData :: m ()
    loadData :: m GameData
