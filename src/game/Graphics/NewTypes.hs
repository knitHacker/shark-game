module Graphics.NewTypes
    ( GView(..)
    , Asset(..)
    , AssetObj(..)
    , AssetScroll(..)
    , AssetMenu(..)
    , AssetMenuItem(..)
    , Overlay(..)
    ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import OutputHandles.Types
import Graphics.Types
import Configs
import InputState

data GView a = GView
    { assets :: [Asset a]
    , overlays :: [Overlay a]
    }

data Overlay a = AOverlay
    { oAssets :: [Asset a]
    , isOverlayActive :: Bool
    }


data Asset a = Asset
    { object :: AssetObj
    , assetX :: Int
    , assetY :: Int
    , assetScale :: Double
    , assetLayer :: Int
    , isVisible :: Bool
    , assetResize :: Maybe (a -> Graphics -> Asset a)
    , assetAnimateStep :: Maybe (a -> Int64 -> Asset a)
    , assetInputUpdate :: Maybe (a -> InputState -> Asset a)
    }

data AssetObj =
      AssetImage Image
    | AssetAnimation Image Int Int
    | AssetText T.Text Color
    | AssetScroll AssetScroll
    | AssetRect Int Int Color
    | AssetMenu AssetMenu
    | AssetStacked [AssetObj] Int -- have the next y be the previous end y + space

data AssetScroll = ScrollObj
    { scrollText :: [T.Text]
    , scrollColor :: Color
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
