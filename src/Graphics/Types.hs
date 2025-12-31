module Graphics.Types
    ( Graphics(..)
    , TextureInfo(..)
    , BlockDrawInfo(..)
    , CursorType(..)
    , Menu(..)
    , MenuAction(..)
    , MenuData(..)
    , MenuOptionType(..)
    , MenuScroll(..)
    , TextOption(..)
    , BasicOption(..)
    , MultiSelectListOptions(..)
    , OverlayMenu(..)
    , OneActionListOptions(..)
    , ColumnButtonOptions(..)
    , ColumnAction(..)
    , SelectOption(..)
    , ScrollListOptions(..)
    , TimeoutData(..)
    , View(..)
    , AnimationData(..)
    , ViewScroll(..)
    , ScrollData(..)
    , MenuPopup(..)
    , TimeoutAction(..)
    , ImageInfo(..)
    , AnimationInfo(..)
    , ImagePlacement(..)
    , AnimPlacement(..)
    , AnimationAction(..)
    , updateView
    , mergeOverlayMenu
    , updateMenuData
    , updateTimeoutData
    , updateMenu
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Int (Int64)

import OutputHandles.Types
import Data.IntMap (update)

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

data OverlayMenu a = Overlay
    { bgXPos :: !Int
    , bgYPos :: !Int
    , bgWidth :: !Int
    , bgHeight :: !Int
    , bgColor :: !Color
    , overlayData :: !(MenuData a)
    } deriving (Show, Eq)

mergeOverlayMenu :: OverlayMenu a -> OverlayMenu a -> OverlayMenu a
mergeOverlayMenu om1 om2 = om2 { overlayData = updateMenuData (overlayData om1) (overlayData om2) }

data TimeoutAction a =
      TimeoutNext a
    | TimeoutAnimation (AnimationData a)
    deriving (Show, Eq)

data TimeoutData a = TimeoutData
    { lastTimeout :: !Int64
    , timeoutLength :: !Int64
    , timeoutAction :: !(TimeoutAction a)
    } deriving (Show, Eq)

updateTimeoutData :: [TimeoutData a] -> [TimeoutData a] -> [TimeoutData a]
updateTimeoutData tds1 tds2 = updateTimeOutData' <$> zip tds1 tds2
    where
        updateTimeOutData' (td1, td2) = td2 { lastTimeout = lastTimeout td1 }

data AnimationAction a = GeneralAnimAction (View a -> Int -> View a) | TextureAnimAction

instance Show (AnimationAction a) where
    show (GeneralAnimAction _) = "GeneralAnimAction <function>"
    show TextureAnimAction = "TextureAnimAction"

instance Eq (AnimationAction a) where
    (GeneralAnimAction _) == (GeneralAnimAction _) = True
    TextureAnimAction == TextureAnimAction = True
    _ == _ = False

data AnimationData a = AnimationData
    { animationFrame :: !Int
    , animationMaxFrames :: !Int
    , animationAction :: AnimationAction a
    } deriving (Show, Eq)

data ScrollData = ScrollData
    { startX :: !Int
    , startY :: !Int
    , vHeight :: !Int
    , scrollHeight :: !Int
    , barHeight :: !Int
    , scrollMaxOffset :: !Int
    } deriving (Show, Eq)

data ViewScroll a = ViewScroll
    { subView :: View a
    , scrollOffset :: !Int
    , scrollMaxY :: !Int
    , scrollStep :: !Int
    , scrollData :: !ScrollData
    } deriving (Show, Eq)

data ImagePlacement = IPlace
    { imgPosX :: !Int
    , imgPosY :: !Int
    , imgScale :: !Double
    , imgTexture :: !Image
    , imgDepth :: !Int
    } deriving (Show, Eq)

data AnimPlacement = APlace
    { animPosX :: !Int
    , animPosY :: !Int
    , animScale :: !Double
    , animTexture :: !Image
    , animFrame :: !Int
    , animDepth :: !Int
    , frameDepth :: !Int
    , animShow :: !Bool
    } deriving (Show, Eq)

data View a = View
    { texts :: ![(TextDisplay, Int)]
    , imgs :: ![ImagePlacement]
    , animations :: ![AnimPlacement]
    , rects :: ![(Color, Int, Int, Int, Int, Int)]
    -- should this be a list? probably but don't have mouse position atm
    , viewScroll :: !(Maybe (ViewScroll a))
    } deriving (Show, Eq)

updateView :: View a -> View a -> View a
updateView (View t1 i1 a1 r1 vs1) (View t2 i2 a2 r2 vs2) =
    View t2 i2 aNew r2 vsNew
    where
        a1Map = M.fromList $ zip [0..] ((\ap-> (animFrame ap, animDepth ap)) <$> a1)
        aNew = zipWith animNew [0..] a2
        animNew idx ap = if idx < length a1
                        then let (f, d) = a1Map M.! idx
                             in ap { animFrame = f, animDepth = d }
                        else ap
        vsNew = case (vs1, vs2) of
            (Just v1, Just v2) -> Just v2 { scrollOffset = scrollOffset v1 }
            _ -> vs2

data MenuAction a = MenuAction
    { menuOptionText :: !T.Text
    , menuNextState :: !(Maybe a)
    } deriving (Show, Eq)

data SelectOption = SelectOption
    { selectOptionText :: !T.Text
    , selectKey :: !T.Text
    , selectSelected :: !Bool
    , selectChangeable :: !Bool
    , selectDisabled :: !Bool
    } deriving (Show, Eq)

data OneActionListOptions a = OALOpts
    { oalOpts :: ![MenuAction a]
    , oalBackOptM :: !(Maybe (MenuAction a))
    , oalUpdateM :: !(Maybe (Int -> a))
    , oalCursor :: !CursorType
    }

instance Eq a => Eq (OneActionListOptions a) where
    (OALOpts opts1 back1 update1 cur1) == (OALOpts opts2 back2 update2 cur2) =
        opts1 == opts2 && back1 == back2 && cur1 == cur2

instance Show a => Show (OneActionListOptions a) where
    show (OALOpts opts back update cur) =
        "OALOpts " ++ show opts ++ " " ++ show back ++ " <update> " ++ show cur

data MultiSelectListOptions a = MSLOpts
    { mslOpts :: ![SelectOption]
    , mslAction :: [T.Text] -> Int -> a
    , mslContinueAction :: Maybe ([T.Text] -> a)
    , mslBackActionM :: !(Maybe a)
    }

instance Eq a => Eq (MultiSelectListOptions a) where
    (MSLOpts opts1 _ _ back1) == (MSLOpts opts2 _ _ back2) =
        opts1 == opts2 && back1 == back2

instance Show a => Show (MultiSelectListOptions a) where
    show (MSLOpts opts _ _ back) =
        "MSLOpts " ++ show opts ++ " <action> <continueAction> " ++ show back

data ColumnAction a = ColAction
    { colOptionTexts :: ![T.Text]
    , colOptionAction :: !(Maybe a)
    } deriving (Show, Eq)

data ColumnButtonOptions a = CBOpts
    { colButOptText :: !T.Text
    , colButOptWidth :: !Int
    , colButOptHeaders :: ![T.Text]
    , colButOptActions :: [ColumnAction a]
    } deriving (Show, Eq)

data TextOption = TextOption
    { textOptionTexts :: ![[T.Text]]
    , textOptionIndent :: !Int
    , textOptionSpace :: !Int
    } deriving (Show, Eq)

data BasicOption a =
      BasicSOALOpts (OneActionListOptions a)
    | BasicMSLOpts (MultiSelectListOptions a)
    | BasicTextOpts TextOption
    | BasicCBOpts (ColumnButtonOptions a)
    deriving (Show, Eq)

data ScrollListOptions a = SLOpts
    { sLScrollOpts :: !(BasicOption a)
    , sLFixedOpts :: ![MenuAction a]
    , sLBackOptM :: !(Maybe (MenuAction a))
    , sLScroll :: !MenuScroll
    } deriving (Show, Eq)

data MenuScroll = Scroll
    { scrollMax :: !Int
    , scrollPos :: !Int
    } deriving (Show, Eq)

optionScroll :: Int -> MenuScroll
optionScroll max = Scroll max 0

data BlockDrawInfo = BlockDrawInfo
    { blockX :: !Int
    , blockY :: !Int
    , blockSize :: !Int
    , blockSpace :: !Int
    } deriving (Show, Eq)

data MenuData a = MenuData
    { menuOptions :: !(MenuOptionType a)
    , menuOptBlockInfo :: !BlockDrawInfo
    , cursorPosition :: !Int
    } deriving (Show, Eq)

updateMenuData :: MenuData a -> MenuData a -> MenuData a
updateMenuData md1 md2 = md2
    { cursorPosition = cursorPosition md1
    , menuOptions = updateMenuOptions (menuOptions md1) (menuOptions md2)
    }
    where
        updateMenuOptions (ScrollListOpts sl1) (ScrollListOpts sl2) =
            ScrollListOpts $ sl2 { sLScroll = updateScroll (sLScroll sl1) (sLScroll sl2) }
        updateMenuOptions _ opts2 = opts2

        updateScroll scroll1 scroll2 = scroll2 { scrollPos = scrollPos scroll1 }

data MenuOptionType a =
      SelOneListOpts (OneActionListOptions a)
    | SelMultiListOpts (MultiSelectListOptions a)
    | ScrollListOpts (ScrollListOptions a)
    deriving (Show, Eq)
    -- todo options at given positions

-- Menu game state
--  Texts are the text to show including where to display
--  Options for actions from this menu
--  Cursor is the current option that is being pointed to
data Menu a = Menu
    { options :: !(MenuData a)
    , popupMaybe :: !(Maybe (MenuPopup a))
    } deriving (Show, Eq)

updateMenu :: Menu a -> Menu a -> Menu a
updateMenu m1 m2 = m2
    { options = updateMenuData (options m1) (options m2)
    , popupMaybe = popM
    }
    where
        popM = case (popupMaybe m1, popupMaybe m2) of
            (Just p1, Just p2) -> Just $ p2 { popupOptions = updateMenuData (popupOptions p1) (popupOptions p2) }
            _ -> popupMaybe m2

data MenuPopup a = MenuPopup
    { popupView :: !(View a)
    , popupOptions :: !(MenuData a)
    , popupX :: !Int
    , popupY :: !Int
    , popupWidth :: !Int
    , popupHeight :: !Int
    , popupColor :: !Color
    } deriving (Show, Eq)

data CursorType = CursorPointer Image | CursorRect Color
                deriving (Show, Eq)
