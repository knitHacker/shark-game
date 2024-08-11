{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module SaveData
    ( GameData
    , startNewGame
    ) where

import System.IO ()
import Paths_shark_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict )
import Data.Aeson.Types ( FromJSON, ToJSON )

import Data.Word
import System.Random.MWC as R
import Control.Monad
import Control.Monad.ST.Lazy (runST)
import Data.Vector.Unboxed
import qualified Data.List as L

data GameData = GameData
    { gameDataSeed :: Seed
    , gameDataFunds :: Int
    , gameDateMonth :: Int
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
    s <- save g
    return $ GameData s 0 0


convertSave :: GameSaveData -> GameData
convertSave gsd = GameData seed (saveFunds gsd) (saveMonth gsd)
    where
        seed = toSeed $ saveSeed gsd

loadFromFile :: FilePath -> IO GameData
loadFromFile fp = do
    gsdM <- eitherDecodeFileStrict fp
    case gsdM of
        Left err -> error ("Failed to open save file " L.++ (show err))
        Right gsd -> return $ convertSave gsd

class Monad m => SaveData m where
    saveData :: m ()
    loadData :: m GameData
