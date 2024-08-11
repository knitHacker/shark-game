{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs
    ( Configs(..)
    , GameConfigs(..)
    , initConfigs
    , ConfigsRead(..)
    , TextureCfg(..)
    ) where

import Control.Monad ()
import System.IO ()
import Paths_shark_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict )
import Data.Aeson.Types ( FromJSON, ToJSON )
import Data.Either ()
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Env.Files    (getGameFullPath)

configFile :: FilePath
configFile = "data/configs/game.json"


texturesFile :: FilePath
texturesFile = "data/configs/textures.json"



data TextureCfg = TextureCfg
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON TextureCfg
instance ToJSON TextureCfg


data PositionCfg = PosCfg
    { x :: Int
    , y :: Int
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON PositionCfg
instance ToJSON PositionCfg

data GameConfigs = GameConfigs
    { debug :: Bool
    , debugOutlineTexture :: Bool
    , debugHitboxes :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    , lastSavePath :: Maybe String
    } deriving (Generic, Show, Eq)


instance FromJSON GameConfigs
instance ToJSON GameConfigs


data Configs = Configs
    { textureCfgs :: M.Map T.Text TextureCfg
    , gameCfgs :: GameConfigs
    } deriving (Generic, Show, Eq)

instance FromJSON Configs
instance ToJSON Configs


initConfigs :: IO Configs
initConfigs = do
    gPath <- getGameFullPath configFile
    configsM <- eitherDecodeFileStrict gPath
    case configsM of
        Left err -> error ("Failed to parse config file: " ++ (show err))
        Right configs -> do
            tPath <- getGameFullPath texturesFile
            texturesM <- eitherDecodeFileStrict tPath
            case texturesM of
                Left err -> error ("Failed to parse texture file: " ++ (show err))
                Right textures -> return $ Configs textures configs


class Monad m => ConfigsRead m where
    readConfigs :: m GameConfigs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug cfgs
