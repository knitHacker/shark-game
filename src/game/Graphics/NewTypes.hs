module Graphics.NewTypes
    ( GView(..)
    , Asset(..)
    , AssetObj(..)
    , AssetScroll(..)
    , AssetMenu(..)
    , AssetMenuItem(..)
    , AssetStackItem(..)
    , Overlay(..)
    , StackDir(..)
    , Resize
    ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import OutputHandles.Types
import Graphics.Types
import Configs
import InputState

data GView = GView
    { assets :: [Asset]
    , overlays :: [Overlay]
    }

data Overlay = AOverlay
    { oAssets :: [Asset]
    , isOverlayActive :: Bool
    }

type Resize = Asset -> Graphics -> Asset

data Asset = Asset
    { object :: AssetObj
    , assetX :: Int
    , assetY :: Int
    , assetLayer :: Int
    , isVisible :: Bool
    , assetResize :: Maybe Resize
--    , assetAnimateStep :: Maybe (a -> Int64 -> Graphics -> Asset a)
    }

data StackDir = StackHorizontal | StackVertical
              deriving (Show, Eq)

data AssetObj =
      AssetImage Image Double
    | AssetAnimation Image Int Int Double
    | AssetText T.Text Color Int
    | AssetScroll AssetScroll
    | AssetRect Int Int Color
    | AssetMenu AssetMenu
    | AssetStacked StackDir [AssetStackItem] Int -- have the next y be the previous end y + space

data AssetStackItem = StackItem
    { stackItem :: AssetObj
    , stackXOff :: Int
    , stackYOff :: Int
    }

data AssetScroll = ScrollObj
    { scrollText :: [T.Text]
    , scrollTextSize :: Int
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
    , menuItemFontSize :: Int
    , highlightedColor :: Maybe Color
    , cursorImg :: Maybe (Image, Double)
    }
