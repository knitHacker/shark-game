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
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Int (Int64)

import OutputHandles.Types


data TextureInfo = TextureInfo
    { textureSizeX :: !Int
    , textureSizeY :: !Int
    }

data Graphics = Graphics
    { graphicsTextures :: M.Map Image TextureInfo
    , graphicsFontSize :: !FontSize -- can be map in the future
    }

data OverlayMenu a = Overlay
    { bgXPos :: !Int
    , bgYPos :: !Int
    , bgWidth :: !Int
    , bgHeight :: !Int
    , bgColor :: !Color
    , overlayData :: !(MenuData a)
    }

data TimeoutData a = TimeoutData
    { lastTimeout :: !Int64
    , timeoutLength :: !Int64
    , timeoutAction :: !a
    }


data AnimationData = AnimationData
    { animationFrame :: !Int
    , animationMaxFrames :: !Int
    } deriving (Show, Eq)


data ScrollData = ScrollData
    { startX :: !Int
    , startY :: !Int
    , vHeight :: !Int
    , scrollHeight :: !Int
    , barHeight :: !Int
    , scrollMaxOffset :: !Int
    }

data ViewScroll a = ViewScroll
    { subView :: View a
    , scrollOffset :: !Int
    , scrollMaxY :: !Int
    , scrollStep :: !Int
    , scrollData :: !ScrollData
    }


data View a = View
    { texts :: ![TextDisplay]
    , imgs :: ![(Int, Int, Double, Image)]
    , rects :: ![(Color, Int, Int, Int, Int)]
    -- should this be a list? probably but don't have mouse position atm
    , viewScroll :: !(Maybe (ViewScroll a))
    }

data MenuAction a = MenuAction
    { menuOptionText :: !T.Text
    , menuNextState :: !(Maybe a)
    }

data SelectOption = SelectOption
    { selectOptionText :: !T.Text
    , selectKey :: !T.Text
    , selectSelected :: !Bool
    , selectChangeable :: !Bool
    , selectDisabled :: !Bool
    }

data OneActionListOptions a = OALOpts
    { oalOpts :: ![MenuAction a]
    , oalCursor :: !CursorType
    }

data MultiSelectListOptions a = MSLOpts
    { mslOpts :: ![SelectOption]
    , mslAction :: [T.Text] -> Int -> a
    , mslContinueAction :: Maybe ([T.Text] -> a)
    , mslBackActionM :: !(Maybe a)
    }

data ColumnAction a = ColAction
    { colOptionTexts :: ![T.Text]
    , colOptionAction :: !(Maybe a)
    }

data ColumnButtonOptions a = CBOpts
    { colButOptText :: !T.Text
    , colButOptWidth :: !Int
    , colButOptHeaders :: ![T.Text]
    , colButOptActions :: [ColumnAction a]
    }

data TextOption = TextOption
    { textOptionTexts :: ![[T.Text]]
    , textOptionIndent :: !Int
    , textOptionSpace :: !Int
    }

data BasicOption a =
      BasicSOALOpts (OneActionListOptions a)
    | BasicMSLOpts (MultiSelectListOptions a)
    | BasicTextOpts TextOption
    | BasicCBOpts (ColumnButtonOptions a)

data ScrollListOptions a = SLOpts
    { sLScrollOpts :: BasicOption a
    , sLFixedOpts :: [MenuAction a]
    , sLScroll :: MenuScroll
    }

data MenuScroll = Scroll
    { scrollMax :: !Int
    , scrollPos :: !Int
    }

optionScroll :: Int -> MenuScroll
optionScroll max = Scroll max 0

data BlockDrawInfo = BlockDrawInfo
    { blockX :: !Int
    , blockY :: !Int
    , blockSize :: !Int
    , blockSpace :: !Int
    }

data MenuData a = MenuData
    { menuOptions :: !(MenuOptionType a)
    , menuOptBlockInfo :: !BlockDrawInfo
    , cursorPosition :: !Int
    }

data MenuOptionType a =
      SelOneListOpts (OneActionListOptions a)
    | SelMultiListOpts (MultiSelectListOptions a)
    | ScrollListOpts (ScrollListOptions a)
    -- todo options at given positions


-- Menu game state
--  Texts are the text to show including where to display
--  Options for actions from this menu
--  Cursor is the current option that is being pointed to
data Menu a = Menu
    { options :: !(MenuData a)
    , popupMaybe :: !(Maybe (MenuPopup a))
    }

data MenuPopup a = MenuPopup
    { popupView :: !(View a)
    , popupOptions :: !(MenuData a)
    , popupX :: !Int
    , popupY :: !Int
    , popupWidth :: !Int
    , popupHeight :: !Int
    , popupColor :: !Color
    }

data CursorType = CursorPointer Image | CursorRect Color
