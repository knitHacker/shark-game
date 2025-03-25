{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GameState.Types
    ( BasicView(..)
    , BlockDrawInfo(..)
    , CursorType(..)
    , GamePlayState(..)
    , GameState(..)
    , GameStateRead(..)
    , GameView(..)
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
    , getNextMenu
    , mkMenu
    , optionLength
    , reDraw
    , selOneOpts
    , selMultOpts
    , scrollOpts
    , toggleMultiOption
    , optionScroll
    , getOptSize
    ) where

import Control.Lens
import Control.Monad ()
import Control.Monad.IO.Class ()
import qualified Data.Map.Strict as M
import Data.Unique ( Unique, hashUnique )
import Data.Int ( Int64 )
import qualified Data.Text as T
import Data.Maybe (isJust)

import InputState
import GameState.Collision.RTree ( RTree )
import GameState.Collision.BoundBox ( BoundBox )
import OutputHandles.Types
import Shark.Types
import SaveData
import Configs
import Util
import SDL (MouseScrollDirection(ScrollFlipped))
import Data.IntMap (update)

instance Show Unique where
    show:: Unique -> String
    show = show . hashUnique

data GameState = GameState
    { gameView :: !GameView
    , gameLastDraw :: !(Maybe ToRender)
    }

data GameView =
      GameMenu !(Maybe OverlayMenu) !Menu
    | OverlayMenu !OverlayMenu !BasicView
    | GameTimeout !(Maybe OverlayMenu) !TimeoutView
    | GameExiting

data BasicView =
      BasicMenu !Menu
    | BasicTimeoutView !TimeoutView

reDraw :: GameView -> GameState
reDraw gv = GameState gv Nothing


data GamePlayState =
      MainMenu GameData
    | PauseMenu GameData GamePlayState
    | IntroPage
    | ResearchCenter GameData
    | TripDestinationSelect GameData
    | TripEquipmentSelect GameData T.Text [T.Text] Int
    | TripReview GameData T.Text [T.Text]
    | TripProgress GameData TripState
    | GameExitState (Maybe GameData)
    | SharkFound GameData (Maybe SharkFind) TripState
    | TripResults GameData TripState
    | DataReviewTop GameData
    | SharkReviewTop GameData (Maybe T.Text)
    | SharkReview GameData (DataEntry SharkInfo)
    | ResearchReviewTop GameData
    | OpenResearchMenu GameData
    | CompletedResearchMenu GameData
    | InvestigateResearchMenu GameData (DataEntry ResearchData)
    | AwardGrantMenu GameData (DataEntry ResearchData)
    | CompletedResearchReviewMenu GameData (DataEntry ResearchData)
    | ComingSoon

data OverlayMenu = Overlay
    { bgXPos :: !Int
    , bgYPos :: !Int
    , bgWidth :: !Int
    , bgHeight :: !Int
    , bgColor :: !Color
    , overlayMenu :: !Menu
    }

data TimeoutView = TimeoutView
    { lastTimeout :: !Int64
    , timeoutLength :: !Int64
    , timeoutView :: !View
    , timeoutAction :: !GamePlayState
    }

mkMenu :: [TextDisplay] -> [(Int, Int, Double, TextureEntry)] -> MenuOptions -> Menu
mkMenu words imgs = Menu (View words imgs [])

data View = View
    { texts :: ![TextDisplay]
    , imgs :: ![(Int, Int, Double, TextureEntry)]
    , rects :: ![(Color, Int, Int, Int, Int)]
    }

data MenuAction = MenuAction
    { menuOptionText :: !T.Text
    , menuOptionEnabled :: !Bool
    , menuNextState :: !GamePlayState
    }

data SelectOption = SelectOption
    { selectOptionText :: !T.Text
    , selectKey :: !T.Text
    , selectSelected :: !Bool
    , changeable :: !Bool
    }

data OneActionListOptions = OALOpts
    { oalOpts :: ![MenuAction]
    , oalCursor :: !CursorType
    }

data MultiSelectListOptions = MSLOpts
    { mslOpts :: ![SelectOption]
    , mslAction :: [T.Text] -> Int -> GamePlayState
    , mslContinueAction :: [T.Text] -> GamePlayState
    , mslBackActionM :: !(Maybe GamePlayState)
    }

data TextOption = TextOption
    { textOptionTexts :: ![[T.Text]]
    , textOptionIndent :: !Int
    , textOptionSpace :: !Int
    }

data BasicOption =
      BasicSOALOpts OneActionListOptions
    | BasicMSLOpts MultiSelectListOptions
    | BasicTextOpts TextOption

data ScrollListOptions = SLOpts
    { sLScrollOpts :: BasicOption
    , sLFixedOpts :: [MenuAction]
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

data MenuOptions = MenuOptions
    { menuOptions :: !MenuOptionType
    , menuOptBlockInfo :: !BlockDrawInfo
    , cursorPosition :: !Int
    }

data MenuOptionType =
      SelOneListOpts OneActionListOptions
    | SelMultiListOpts MultiSelectListOptions
    | ScrollListOpts ScrollListOptions
    -- todo options at given positions

selOneOpts :: Int -> Int -> Int -> Int -> [MenuAction] -> CursorType -> Int -> MenuOptions
selOneOpts x y s sp opts curs = MenuOptions (SelOneListOpts $ OALOpts opts curs) (BlockDrawInfo x y s sp)

selMultOpts :: Int -> Int -> Int -> Int -> [SelectOption]
            -> ([T.Text] -> Int -> GamePlayState)
            -> ([T.Text] -> GamePlayState)
            -> Maybe GamePlayState -> Int -> MenuOptions
selMultOpts x y s sp opts up act back = MenuOptions (SelMultiListOpts $ MSLOpts opts up act back) (BlockDrawInfo x y s sp)

scrollOpts :: Int -> Int -> Int -> Int -> BasicOption -> [MenuAction] -> Int -> Int -> MenuOptions
scrollOpts x y s sp opts fixed maxScroll pos = MenuOptions (ScrollListOpts $ SLOpts opts fixed (Scroll maxScroll pos)) (BlockDrawInfo x y s sp) pos

getNextMenu :: MenuOptions -> Maybe GamePlayState
getNextMenu (MenuOptions (SelOneListOpts opts) _ pos) = getNextOALOpts opts pos
getNextMenu (MenuOptions (SelMultiListOpts opts) _ pos) = getNextMSLOpts opts pos
getNextMenu (MenuOptions (ScrollListOpts (SLOpts opts fixed _)) _ pos)
    | pos < optLen = getNextOpt opts pos
    | otherwise = if menuOptionEnabled opt then Just $ menuNextState opt else Nothing
    where
        optLen = getOptSize opts
        opt = fixed !! (pos - optLen)

getNextOpt :: BasicOption-> Int -> Maybe GamePlayState
getNextOpt (BasicSOALOpts opts) pos = getNextOALOpts opts pos
getNextOpt (BasicMSLOpts opts) pos = getNextMSLOpts opts pos
getNextOpt _ _ = Nothing

getOptSize :: BasicOption -> Int
getOptSize (BasicSOALOpts opts) = length $ oalOpts opts
getOptSize (BasicMSLOpts opts) = length $ mslOpts opts
getOptSize (BasicTextOpts to) = length $ textOptionTexts to

getNextOALOpts :: OneActionListOptions -> Int -> Maybe GamePlayState
getNextOALOpts (OALOpts opts _) pos = if menuOptionEnabled opt then Just $ menuNextState opt else Nothing
    where
        opt = opts !! pos

getNextMSLOpts :: MultiSelectListOptions -> Int -> Maybe GamePlayState
getNextMSLOpts opts pos
    | pos < len = Just $ mslAction opts selected pos
    | len == pos = Just $ mslContinueAction opts selected
    | otherwise = case mslBackActionM opts of
                    Nothing -> Just $ mslAction opts selected pos
                    Just back -> Just back
    where
        len = length (mslOpts opts)
        opts' = toggleMultiOption opts pos
        selected = selectKey <$> filter selectSelected (mslOpts opts')

optionLength :: MenuOptions -> Int
optionLength (MenuOptions (SelOneListOpts opts) _ _) = length $ oalOpts opts
optionLength (MenuOptions (SelMultiListOpts opts) _ _) = 1 + length (mslOpts opts) + if isJust (mslBackActionM opts) then 1 else 0
optionLength (MenuOptions (ScrollListOpts opts) _ _) = length (sLFixedOpts opts) + case sLScrollOpts opts of
    BasicSOALOpts oal -> length $ oalOpts oal
    BasicMSLOpts msl -> 1 + length (mslOpts msl) + if isJust (mslBackActionM msl) then 1 else 0
    BasicTextOpts to -> length $ textOptionTexts to

toggleMultiOption :: MultiSelectListOptions -> Int -> MultiSelectListOptions
toggleMultiOption opt pos = opt { mslOpts = (\(n, o) -> if n == pos then toggle o else o) <$> zip [0..] (mslOpts opt)}
    where
        toggle (SelectOption t k True True) = SelectOption t k False True
        toggle (SelectOption t k False True) = SelectOption t k True True
        toggle sopt = sopt

-- Menu game state
--  Texts are the text to show including where to display
--  Options for actions from this menu
--  Cursor is the current option that is being pointed to
data Menu = Menu
    { menuView :: !View
    , options :: !MenuOptions
    }

data CursorType = CursorPointer TextureEntry | CursorRect Color

type Barriers = RTree ()

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

