{-# LANGUAGE Strict #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GameState.Types
    ( BasicView(..)
    , CursorType(..)
    , GamePlayState(..)
    , GameState(..)
    , GameStateRead(..)
    , GameView(..)
    , Menu(..)
    , MenuAction(..)
    , MenuOptions(..)
    , MenuOptionType(..)
    , MultiSelectListOptions(..)
    , OverlayMenu(..)
    , OneActionListOptions(..)
    , SelectOption(..)
    , TimeoutView(..)
    , View(..)
    , getNextMenu
    , mkMenu
    , optionLength
    , reDraw
    , selOneOpts
    , selMultOpts
    , toggleMultiOption
    , optionScroll
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
    | SharkReviewTop GameData
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

mkMenu :: [TextDisplay] -> [(Int, Int, TextureEntry)] -> MenuOptionType -> Int -> Menu
mkMenu words imgs opts pos = Menu (View words imgs []) (MenuOptions opts pos Nothing)

mkMenuScroll :: [TextDisplay] -> [(Int, Int, TextureEntry)] -> MenuOptionType -> Int -> Int -> Menu
mkMenuScroll words imgs opts pos m = Menu (View words imgs []) (MenuOptions opts pos (Just $ optionScroll m))

data View = View
    { texts :: ![TextDisplay]
    , imgs :: ![(Int, Int, TextureEntry)]
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
    { oalXPos :: !Int
    , oalYPos :: !Int
    , oalSize :: !Int
    , oalSpace :: !Int
    , oalOpts :: ![MenuAction]
    , oalCursor :: !CursorType
    }

data MultiSelectListOptions = MSLOpts
    { mslXPos :: !Int
    , mslYPos :: !Int
    , mslSize :: !Int
    , mslSpace :: !Int
    , mslOpts :: ![SelectOption]
    , mslAction :: [T.Text] -> Int -> GamePlayState
    , mslContinueAction :: [T.Text] -> GamePlayState
    , mslBackActionM :: !(Maybe GamePlayState)
    }

data MenuScroll = Scroll
    { scrollMax :: !Int
    , scrollPos :: !Int
    }

optionScroll :: Int -> MenuScroll
optionScroll max = Scroll max 0

data MenuOptions = MenuOptions
    { menuOptions :: !MenuOptionType
    , currentPos :: !Int
    , menuScroll :: Maybe MenuScroll
    }

data MenuOptionType =
      SelOneListOpts OneActionListOptions
    | SelMultiListOpts MultiSelectListOptions
    -- todo options at given positions


selOneOpts :: Int -> Int -> Int -> Int -> [MenuAction] -> CursorType -> MenuOptionType
selOneOpts x y s sp opts curs = SelOneListOpts $ OALOpts x y s sp opts curs

selMultOpts :: Int -> Int -> Int -> Int -> [SelectOption] -> ([T.Text] -> Int -> GamePlayState) -> ([T.Text] -> GamePlayState) -> Maybe GamePlayState -> MenuOptionType
selMultOpts x y s sp opts up act back = SelMultiListOpts $ MSLOpts x y s sp opts up act back

getNextMenu :: MenuOptions -> Maybe GamePlayState
getNextMenu (MenuOptions (SelOneListOpts opts) pos _) = if menuOptionEnabled opt then Just $ menuNextState opt else Nothing
        where
            opt = oalOpts opts !! pos
getNextMenu (MenuOptions (SelMultiListOpts opts) pos _)
    | pos < len = Just $ mslAction opts selected pos
    | len == pos = Just $ mslContinueAction opts selected
    | otherwise =
        case mslBackActionM opts of
            Nothing -> Just $ mslAction opts selected pos
            Just back -> Just back
    where
        len = length (mslOpts opts)
        opts' = toggleMultiOption opts pos
        selected = selectKey <$> filter selectSelected (mslOpts opts')

optionLength :: MenuOptions -> Int
optionLength (MenuOptions (SelOneListOpts opts) _ _) = length $ oalOpts opts
optionLength (MenuOptions (SelMultiListOpts opts) _ _) = 1 + length (mslOpts opts) + if isJust (mslBackActionM opts) then 1 else 0

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

