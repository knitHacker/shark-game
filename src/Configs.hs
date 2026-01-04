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
import Data.Aeson ( FromJSON, ToJSON )
import Data.Aeson.Types ( FromJSON, ToJSON )
import Data.Yaml ( decodeFileEither, encodeFile, ParseException )
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
configFile = "data/configs/game.yaml"

texturesFile :: FilePath
texturesFile = "data/configs/textures.yaml"

stateFile :: FilePath
stateFile = "data/configs/state.yaml"

sharkFile :: FilePath
sharkFile = "data/configs/shark_game.yaml"

researchFile :: FilePath
researchFile = "data/configs/research.yaml"

mechanicsFile :: FilePath
mechanicsFile = "data/configs/mechanics.yaml"

-- Wrapper types for individual config files
data ResearchConfig = ResearchConfig 
    { research :: M.Map T.Text ResearchData 
    } deriving (Generic, Show, Eq)

instance FromJSON ResearchConfig
instance ToJSON ResearchConfig

data GameMechanicsConfig = GameMechanicsConfig
    { gameMechanics :: GameMechanics
    } deriving (Generic, Show, Eq)

instance FromJSON GameMechanicsConfig
instance ToJSON GameMechanicsConfig

data GamePlayConfig = GamePlayConfig
    { boats :: M.Map T.Text Boat
    , equipment :: M.Map T.Text GameEquipment
    , regions :: M.Map T.Text SiteLocation
    , sharks :: M.Map T.Text SharkInfo
    } deriving (Generic, Show, Eq)

instance FromJSON GamePlayConfig
instance ToJSON GamePlayConfig


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
    } deriving (Show, Eq)

checkConfigs :: Configs -> Bool
checkConfigs cfgs = checkPlayConfigs (pCfgs cfgs)

-- Helper to convert ParseException to String
decodeYaml :: FromJSON a => FilePath -> IO (Either String a)
decodeYaml path = do
    result <- decodeFileEither path
    return $ case result of
        Left err -> Left (show err)
        Right val -> Right val

loadOrCreate :: (FromJSON a, ToJSON a, Default a) => FilePath -> IO (Either String a)
loadOrCreate path = do
        exists <- doesFileExist path
        if exists
            then decodeYaml path
            else do
                let dir = takeDirectory path
                let newState = def
                createDirectoryIfMissing True dir
                encodeFile path newState
                return $ Right newState

initConfigs :: IO (TextureCfg, GameConfigs)
initConfigs = do
    gPath <- getGameFullPath configFile
    configsE <- decodeYaml gPath
    tPath <- getGameFullPath texturesFile
    texturesE <- decodeYaml tPath
    sPath <- getLocalGamePath stateFile
    stateE <- loadOrCreate sPath
    pPath <- getGameFullPath sharkFile
    rPath <- getGameFullPath researchFile
    mPath <- getGameFullPath mechanicsFile
    gamePlayE <- decodeYaml pPath :: IO (Either String GamePlayConfig)
    researchE <- decodeYaml rPath :: IO (Either String ResearchConfig)
    mechanicsE <- decodeYaml mPath :: IO (Either String GameMechanicsConfig)
    let playConfigsE = buildPlayConfigs <$> gamePlayE <*> researchE <*> mechanicsE
    let cfgE = Right Configs <*> texturesE <*> configsE <*> stateE <*> playConfigsE
    case cfgE of
        Left err -> error ("Faile to parse game configs: " ++ show err)
        Right configs -> if checkConfigs configs
                            then return (textureCfgs configs, toGameConfigs configs)
                            else error "Invalid game configs"
  where
    buildPlayConfigs :: GamePlayConfig -> ResearchConfig -> GameMechanicsConfig -> PlayConfigs
    buildPlayConfigs (GamePlayConfig b e r s) (ResearchConfig res) (GameMechanicsConfig gm) = PlayConfigs
        { boats = b
        , equipment = e
        , regions = r
        , sharks = s
        , research = res
        , gameMechanics = gm
        }


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
