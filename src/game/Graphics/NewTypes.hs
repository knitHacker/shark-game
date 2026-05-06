module Graphics.NewTypes
    ( GView(..)
    , Asset(..)
    , AssetObj(..)
    , AssetScroll(..)
    , MenuAsset(..)
    , AssetMenuItem(..)
    , AssetStackItem(..)
    , Overlay(..)
    , StackDir(..)
    , Resize
    , AssetId
    , AnimationState(..)
    ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import OutputHandles.Types
import Graphics.Types
import Configs
import InputState

type AssetId = Int

--data MenuState a = MState
--    { curr :: a
--    }

data AnimationState = AnimState
    { lastAnimTS :: Int64
    , updateInterval :: Int
    , animationAssets :: [AssetId]
    , animationUpdate :: (Asset -> Graphics -> Asset)
    }

data GView = GView
    { assets :: M.Map AssetId Asset
    , overlays :: M.Map Int Overlay
    , activeOverlays :: [Int]
    , menuAsset :: Maybe MenuAsset
    }

data Overlay = AOverlay
    { oAssets :: M.Map Int Asset
    , oMenu :: Maybe MenuAsset -- Does this need to not be maybe?
    , overlayX :: Int
    , overlayY :: Int
    , overlayW :: Int
    , overlayH :: Int
    , overlayColor :: Color
    , resizeOverlay :: Maybe (Overlay -> Graphics -> Overlay)
    }

type Resize = Asset -> Graphics -> Asset

data Asset = Asset
    { object :: AssetObj
    , assetX :: Int
    , assetY :: Int
    , assetLayer :: Int
    , isVisible :: Bool
    , assetResize :: Maybe Resize
    }

data StackDir = StackHorizontal | StackVertical
              deriving (Show, Eq)

data AssetObj =
      AssetImage Image Double
    | AssetAnimation Image Int Int Double
    | AssetText T.Text Color Int
    | AssetScroll AssetScroll
    | AssetRect Int Int Color
    | AssetStacked StackDir [AssetStackItem] Int -- have the next y be the previous end y + space
    | AssetEmpty

data AssetStackItem = StackItem
    { stackItem :: AssetObj
    , stackXOff :: Int
    , stackYOff :: Int
    }

data AssetScroll = ScrollObj
    { scrollAssets :: M.Map AssetId Asset
    , scrollPosition :: Int
    , scrollHeight :: Int
    }

data MenuAsset = MenuAsset
    { menuXBase :: Int
    , menuYBase :: Int
    , menuLayer :: Int
    , menuResize :: Maybe (MenuAsset -> Graphics -> MenuAsset)
    , showScroll :: Bool
    , menuLineSpace :: Int
    , menuItems :: [AssetMenuItem]
    }

data AssetMenuItem = MenuItem
    { menuItemText :: T.Text
    , menuItemColor :: Color
    , menuItemFontSize :: Int
    , menuItemXOff :: Int
    , menuItemYOff :: Int
    , menuItemVisible :: Bool
    , highlightedColor :: Maybe Color
    , cursorImg :: Maybe (Image, Double)
    }
