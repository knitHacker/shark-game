module Graphics.Types
    ( Graphics(..)
    , TextureInfo(..)
    , ImageInfo(..)
    , AnimationInfo(..)
    , GraphicsRead(..)
    , GraphicsUpdate(..)
    , GView(..)
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
    , AssetMenu(..)
    , ScrollMenuAsset(..)
    , ScrollMenuItem(..)
    , MenuText(..)
    , AssetStack(..)
    ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import OutputHandles.Types

data ImageInfo = ImageInfo
    { imageSizeX :: !Int
    , imageSizeY :: !Int
    } deriving (Show, Eq)

data AnimationInfo = AnimationInfo
    { animSizeX :: !Int
    , animSizeY :: !Int
    , animFrameCount :: !Int
    , animFrameDepth :: !Int
    } deriving (Show, Eq)

data TextureInfo = ImageCfg ImageInfo | AnimationCfg AnimationInfo
                   deriving (Show, Eq)

data Graphics = Graphics
    { graphicsStaticTextures :: !(M.Map Image ImageInfo)
    , graphicsAnimTextures :: !(M.Map Image AnimationInfo)
    , graphicsFontSize :: !FontSize -- can be map in the future
    , graphicsWindowWidth :: !Int
    , graphicsWindowHeight :: !Int
    } deriving (Show, Eq)

class Monad m => GraphicsRead m where
    readGraphics :: m Graphics

class Monad m => GraphicsUpdate m where
    updateWindowSize :: Maybe (Int, Int) -> m ()
    updateFont :: FontSize -> m ()

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
    , overlays :: S.Set Overlay
    , menuAsset :: Maybe AssetMenu
    }

data Overlay = AOverlay
    { oKey :: Int
    , oAssets :: M.Map Int Asset
    , oMenu :: Maybe AssetMenu -- Does this need to not be maybe?
    , overlayX :: Int
    , overlayY :: Int
    , overlayW :: Int
    , overlayH :: Int
    , overlayColor :: Color
    , resizeOverlay :: Maybe (Overlay -> Graphics -> Overlay)
    }

instance Eq Overlay where
    (==) o1 o2 = oKey o1 == oKey o2

instance Ord Overlay where
    compare o1 o2
        | o1 == o2  = EQ
        | oKey o1 > oKey o2 = LT
        | otherwise = GT

type Resize = Asset -> Graphics -> Asset

data Asset = Asset
    { object :: AssetObj
    , assetX :: Int
    , assetY :: Int
    , assetLayer :: Int
    , isVisible :: Bool
    , assetResize :: Maybe Resize
    }

data StackDir = StackHorizontal | StackVertical | AbsoluteHorizontal | AbsoluteVertical
              deriving (Show, Eq)

data AssetObj =
      AssetImage Image Double
    | AssetAnimation Image Int Int Double
    | AssetText T.Text Color Int
    | AssetScroll AssetScroll
    | AssetRect Int Int Color
    | AssetStacked AssetStack
    | AssetEmpty

data AssetStack = AssetStack
    { stackDir :: StackDir
    , stackItems :: [AssetStackItem]
    , stackSpace :: Int
    }

data AssetStackItem = StackItem
    { stackItem :: AssetObj
    , stackXOff :: Int
    , stackYOff :: Int
    }

data AssetScroll = ScrollObj
    { scrollAssets :: M.Map AssetId Asset
    , scrollPosition :: Int
    , scrollSeenHeight :: Int
    }

data MenuText = MText T.Text | MRow [(T.Text, Int, Maybe Color, Bool)]
data AssetMenu = ScrollMenu ScrollMenuAsset | DefaultMenu MenuAsset

data ScrollMenuAsset = ScrollMenuAsset
    { menuScrollX :: Int
    , menuScrollY :: Int
    , menuScrollLayer :: Int
    , menuScrollResize :: Maybe (ScrollMenuAsset -> Graphics -> ScrollMenuAsset)
    , menuScrollLineSpace :: Int
    , menuFontSize :: Int
    , menuScrollItems :: [ScrollMenuItem]
    , menuScrollStartIdx :: Int
    , menuScrollViewable :: Int
    , menuStationaryItems :: [AssetMenuItem]
    }

data ScrollMenuItem = ScrollMenuItem
    { menuScrollItemText :: MenuText
    , menuScrollItemColor :: Color
    , menuScrollXOff :: Int
    , highlightedScrollColor :: Maybe Color
    , cursorScrollImg :: Maybe (Image, Double)
    }

data MenuAsset = MenuAsset
    { menuXBase :: Int
    , menuYBase :: Int
    , menuLayer :: Int
    , menuResize :: Maybe (MenuAsset -> Graphics -> MenuAsset)
    , menuLineSpace :: Int
    , menuItems :: [AssetMenuItem]
    }

data AssetMenuItem = MenuItem
    { menuItemText :: MenuText
    , menuItemColor :: Color
    , menuItemFontSize :: Int
    , menuItemXOff :: Int
    , menuItemYOff :: Int
    , menuItemVisible :: Bool
    , highlightedColor :: Maybe Color
    , cursorImg :: Maybe (Image, Double)
    }
