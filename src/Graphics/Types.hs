module Graphics.Types
    ( Graphics(..)
    , TextureInfo(..)
    , BasicView(..)
    , BlockDrawInfo(..)
    , CursorType(..)
    , Menu(..)
    , MenuAction(..)
    , MenuOptions(..)
    , MenuOptionType(..)
    , MenuScroll(..)
    , TextOption(..)
    , BasicOption(..)
    , MultiSelectListOptions(..)
    , OverlayMenu(..)
    , OneActionListOptions(..)
    , SelectOption(..)
    , ScrollListOptions(..)
    , TimeoutView(..)
    , View(..)
    , ViewScroll(..)
    , ScrollData(..)
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

data BasicView a =
      BasicMenu !(Menu a)
    | BasicTimeoutView !(TimeoutView a)

data OverlayMenu a = Overlay
    { bgXPos :: !Int
    , bgYPos :: !Int
    , bgWidth :: !Int
    , bgHeight :: !Int
    , bgColor :: !Color
    , overlayMenu :: !(Menu a)
    }

data TimeoutView a = TimeoutView
    { lastTimeout :: !Int64
    , timeoutLength :: !Int64
    , timeoutView :: !View
    , timeoutAction :: !a
    }

data ScrollData = ScrollData
    { startY :: !Int
    , height :: !Int
    , step :: !Int
    }

data ViewScroll = ViewScroll
    { subView :: View
    , scrollOffset :: !Int
    , scrollMaxHeight :: !Int
    --, scrollData :: !ScrollData
    }


data View = View
    { texts :: ![TextDisplay]
    , imgs :: ![(Int, Int, Double, Image)]
    , rects :: ![(Color, Int, Int, Int, Int)]
    -- should this be a list? probably but don't have mouse position atm
    , viewScroll :: !(Maybe ViewScroll)
    }

data MenuAction a = MenuAction
    { menuOptionText :: !T.Text
    , menuOptionEnabled :: !Bool
    , menuNextState :: !a
    }

data SelectOption = SelectOption
    { selectOptionText :: !T.Text
    , selectKey :: !T.Text
    , selectSelected :: !Bool
    , changeable :: !Bool
    }

data OneActionListOptions a = OALOpts
    { oalOpts :: ![MenuAction a]
    , oalCursor :: !CursorType
    }

data MultiSelectListOptions a = MSLOpts
    { mslOpts :: ![SelectOption]
    , mslAction :: [T.Text] -> Int -> a
    , mslContinueAction :: [T.Text] -> a
    , mslBackActionM :: !(Maybe a)
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

data MenuOptions a = MenuOptions
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
    { menuView :: !View
    , options :: !(MenuOptions a)
    }

data CursorType = CursorPointer Image | CursorRect Color
