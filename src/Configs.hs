{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Configs
    ( ConfigsRead(..)
    , GameConfigs(..)
    , StateConfigs(..)
    , SettingConfigs(..)
    , ImageTexture(..)
    , AnimationTexture(..)
    , TextureFileMap
    , TextureCfg(..)
    , initConfigs
    , updateStateConfigs
    , getTextureFiles
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
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.Default

import Env.Files    (getGameFullPath, getLocalGamePath)
import Shark.Types
import Data.Graph (path)
import SDL.Font (load)

configFile :: FilePath
configFile = "data/configs/game.json"

texturesFile :: FilePath
texturesFile = "data/configs/textures.json"

stateFile :: FilePath
stateFile = "data/configs/state.json"

sharkFile :: FilePath
sharkFile = "data/configs/shark_game.json"


type TextureFileMap = M.Map T.Text FilePath

getTextureFiles :: TextureCfg -> TextureFileMap
getTextureFiles tc = M.union imgFiles animFiles
  where
    imgFiles = M.map (\(ImageTexture _ _ f) -> f) (textureImages tc)
    animFiles = M.map (\(AnimationTexture _ _ f _ _) -> f) (textureAnimations tc)

data ImageTexture = ImageTexture
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON ImageTexture
instance ToJSON ImageTexture

data AnimationTexture = AnimationTexture
    { sizeX :: Int
    , sizeY :: Int
    , file :: FilePath
    , frames :: Int
    , depth :: Int
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON AnimationTexture
instance ToJSON AnimationTexture

data TextureCfg = TextureCfg
    { textureImages :: M.Map T.Text ImageTexture
    , textureAnimations :: M.Map T.Text AnimationTexture
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
    , windowSizeX :: Int
    , windowSizeY :: Int
    , fontSize :: Int
    , frameRate :: Word32
    , fullScreen :: Bool
    } deriving (Generic, Show, Eq)


instance FromJSON SettingConfigs
instance ToJSON SettingConfigs

data StateConfigs = StateConfigs
    { lastSaveM :: Maybe String
    } deriving (Generic, Show, Eq)

instance FromJSON StateConfigs
instance ToJSON StateConfigs

instance Default StateConfigs where
    def :: StateConfigs
    def = StateConfigs Nothing

data Configs = Configs
    { textureCfgs :: !TextureCfg
    , gameCfgs :: !SettingConfigs
    , sCfgs :: !StateConfigs
    , pCfgs :: !PlayConfigs
    } deriving (Generic, Show, Eq)

instance FromJSON Configs
instance ToJSON Configs

checkConfigs :: Configs -> Bool
checkConfigs cfgs = checkPlayConfigs (pCfgs cfgs)

loadOrCreate :: (FromJSON a, ToJSON a, Default a) => FilePath -> IO (Either String a)
loadOrCreate path = do
        exists <- doesFileExist path
        if exists
            then eitherDecodeFileStrict path
            else do
                let dir = takeDirectory path
                let newState = def
                createDirectoryIfMissing True dir
                encodeFile path newState
                return $ Right newState

initConfigs :: IO (TextureCfg, GameConfigs)
initConfigs = do
    gPath <- getGameFullPath configFile
    configsE <- eitherDecodeFileStrict gPath
    tPath <- getGameFullPath texturesFile
    texturesE <- eitherDecodeFileStrict tPath
    sPath <- getLocalGamePath stateFile
    stateE <- loadOrCreate sPath
    pPath <- getGameFullPath sharkFile
    playerE <- eitherDecodeFileStrict pPath
    let cfgE = Right Configs <*> texturesE <*> configsE <*> stateE <*> playerE
    case cfgE of
        Left err -> error ("Faile to parse game configs: " ++ show err)
        Right configs -> if checkConfigs configs
                            then return (textureCfgs configs, toGameConfigs configs)
                            else error "Invalid game configs"


toGameConfigs :: Configs -> GameConfigs
toGameConfigs (Configs _ g s p) = GameConfigs g s p

updateStateConfigs :: StateConfigs -> IO ()
updateStateConfigs sc = do
    sPath <- getLocalGamePath stateFile
    encodeFile sPath sc

class Monad m => ConfigsRead m where
    readConfigs :: m GameConfigs

    readFrameRate :: m Word32
    readFrameRate = do
        cfgs <- readConfigs
        return $ frameRate $ settingCfgs cfgs

    debugMode :: m Bool
    debugMode = do
        cfgs <- readConfigs
        return $ debug $ settingCfgs cfgs
