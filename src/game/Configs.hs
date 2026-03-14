{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Configs
    ( GameConfigs(..)
    , SettingConfigs(..)
    , ImageTexture(..)
    , AnimationTexture(..)
    , TextureFileMap
    , TextureCfg(..)
    , getTextureFiles
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Aeson.Types ( FromJSON, ToJSON )
import Data.Word ( Word32 )
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Shark.Types


-- Wrapper types for individual config files
type ResearchConfig = M.Map T.Text ResearchData

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


data Configs = Configs
    { textureCfgs :: !TextureCfg
    , gameCfgs :: !SettingConfigs
    , pCfgs :: !PlayConfigs
    } deriving (Show, Eq)

checkConfigs :: Configs -> Bool
checkConfigs cfgs = checkPlayConfigs (pCfgs cfgs)


toGameConfigs :: Configs -> GameConfigs
toGameConfigs (Configs _ g p) = GameConfigs g p
