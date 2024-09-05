{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs
    ( ConfigsRead(..)
    , GameConfigs(..)
    , StateConfigs(..)
    , SettingConfigs(..)
    , TextureCfg(..)
    , TextureFileMap
    , initConfigs
    , updateStateConfigs
    ) where

import Control.Monad ()
import System.IO ()
import Paths_shark_game ()
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import Data.Either ()
import Data.Word (Word32)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Env.Files    (getGameFullPath)
import Shark.Types

configFile :: FilePath
configFile = "data/configs/game.json"

texturesFile :: FilePath
texturesFile = "data/configs/textures.json"

stateFile :: FilePath
stateFile = "data/configs/state.json"

sharkFile :: FilePath
sharkFile = "data/configs/shark_game.json"

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
    { settingCfgs :: SettingConfigs
    , stateCfgs :: StateConfigs
    , sharkCfgs :: PlayConfigs
    } deriving (Show, Eq)

data SettingConfigs = SettingConfigs
    { debug :: Bool
    , debugOutlineTexture :: Bool
    , debugHitboxes :: Bool
    , boardSizeX :: Int
    , boardSizeY :: Int
    , windowSizeX :: Int
    , windowSizeY :: Int
    } deriving (Generic, Show, Eq)


instance FromJSON SettingConfigs
instance ToJSON SettingConfigs

data StateConfigs = StateConfigs
    { lastSaveM :: Maybe String
    } deriving (Generic, Show, Eq)

instance FromJSON StateConfigs
instance ToJSON StateConfigs

type TextureFileMap = M.Map T.Text TextureCfg

data Configs = Configs
    { textureCfgs :: !TextureFileMap
    , gameCfgs :: !SettingConfigs
    , sCfgs :: !StateConfigs
    , pCfgs :: !PlayConfigs
    } deriving (Generic, Show, Eq)

instance FromJSON Configs
instance ToJSON Configs


initConfigs :: IO (TextureFileMap, GameConfigs)
initConfigs = do
    gPath <- getGameFullPath configFile
    configsE <- eitherDecodeFileStrict gPath
    tPath <- getGameFullPath texturesFile
    texturesE <- eitherDecodeFileStrict tPath
    sPath <- getGameFullPath stateFile
    stateE <- eitherDecodeFileStrict sPath
    pPath <- getGameFullPath sharkFile
    playerE <- eitherDecodeFileStrict pPath
    let cfgE = Right Configs <*> texturesE <*> configsE <*> stateE <*> playerE
    case cfgE of
        Left err -> error ("Faile to parse game configs: " ++ (show err))
        Right configs -> return (textureCfgs configs, toGameConfigs configs)


toGameConfigs :: Configs -> GameConfigs
toGameConfigs (Configs _ g s p) = GameConfigs g s p

updateStateConfigs :: StateConfigs -> IO ()
updateStateConfigs sc = do
    sPath <- getGameFullPath stateFile
    encodeFile sPath sc

class Monad m => ConfigsRead m where
    readConfigs :: m GameConfigs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug $ settingCfgs cfgs
