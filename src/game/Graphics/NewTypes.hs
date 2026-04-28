module Graphics.NewTypes
    ( GView(..)
    , Asset(..)
    , AssetObj(..)
    , AssetScroll(..)
    , AssetMenu(..)
    , AssetMenuItem(..)
    , staticAsset
    ) where

import Data.Int (Int64)
import qualified Data.Text as T

import OutputHandles.Types
import Graphics.Types
import Configs
import InputState

data GView a = GView
    { assets :: [Asset a]
    }


staticAsset :: AssetObj -> Int -> Int -> Double -> Int -> Asset a
staticAsset o x y s l = Asset o x y s l Nothing Nothing Nothing

data Asset a = Asset
    { object :: AssetObj
    , assetX :: Int
    , assetY :: Int
    , assetScale :: Double
    , assetLayer :: Int
    , assetResize :: Maybe (a -> Graphics -> Asset a)
    , assetAnimateStep :: Maybe (a -> Int64 -> Asset a)
    , assetInputUpdate :: Maybe (a -> InputState -> Asset a)
    }

data AssetObj =
      AssetImage Image
    | AssetAnimation Image
    | AssetText T.Text Color
    | AssetScroll AssetScroll
    | AssetRect Int Int Color
    | AssetMenu AssetMenu

data AssetScroll = ScrollObj
    { scrollText :: [T.Text]
    , showTextScroll :: Bool
    , scrollLineSpace :: Int
    }

data AssetMenu = MenuObj
    { menuItems :: [AssetMenuItem]
    , showScroll :: Bool
    , menuLineSpace :: Int
    }

data AssetMenuItem = MenuItem
    { menuItemText :: T.Text
    , menuItemColor :: Color
    , highlightedColor :: Color
    , isHighlighted :: Bool
    }