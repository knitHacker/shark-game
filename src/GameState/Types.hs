{-# LANGUAGE Strict #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module GameState.Types
    ( GameState(..)
    , GameStateRead(..)
    , Menu(..)
    , MenuAction(..)
    , MenuOptions(..)
    , OverlayMenu(..)
    , GamePlayState(..)
    , CursorType(..)
    , OneActionListOptions(..)
    , MultiSelectListOptions(..)
    , SelectOption(..)
    , getNextMenu
    , selOneOpts
    , selMultOpts
    , optionLength
    , toggleMultiOption
    , reDraw
    , GameView(..)
    , noUpdate
    , TripState(..)
    , initTripProgress
    ) where

import Control.Lens
import Control.Monad ()
import Control.Monad.IO.Class ()
import qualified Data.Map.Strict as M
import Data.Unique ( Unique, hashUnique )
import Data.Word ( Word32 )
import qualified Data.Text as T
import InputState
import Data.Maybe (isJust)

import Configs ( GameConfigs )
import InputState ( Direction, InputState )
import OutputHandles.Types
    ( OutputHandles
    , TextureEntry
    , TextDisplay
    , Color(..)
    )
import GameState.Collision.RTree ( RTree )
import GameState.Collision.BoundBox ( BoundBox )
import Shark.Trip
import SaveData
import Configs

instance Show Unique where
    show:: Unique -> String
    show = show . hashUnique

-- Top level game state
--  Game menu is a menu with different options
--  Game state is where character walks around
--  Game exiting is how tell top loop to quit
data GameState = GameState
    { gameView :: !GameView
    , gameReDraw :: !Bool
    , gameDrawCount :: !Int
    }

data GameView =
      GameView !(Maybe OverlayMenu) !Menu
    | OverlayMenu !OverlayMenu !Menu
    | GameExiting

reDraw :: GameView -> GameState
reDraw gv = GameState gv True 0

noUpdate :: GameState -> GameState
noUpdate (GameState gv _ gdc)
    | gdc < 30 = GameState gv False (gdc + 1)
    | otherwise = reDraw gv


data TripState = TripState
    { trip :: TripInfo
    , attempts :: Int
    , tripTotalTries :: Int
    , sharkFinds :: [SharkFind]
    }

initTripProgress :: GameData -> GameLocation -> [T.Text] -> GameConfigs -> (GameData, TripState)
initTripProgress gd loc eqKeys cfgs = (gd', TripState trip 0 atmpts [])
    where
        playCfgs = sharkCfgs cfgs
        trip = tripInfo playCfgs loc eqKeys
        (gd', atmpts) = catchAttempts playCfgs gd trip


data GamePlayState =
      MainMenu GameData
    | PauseMenu GameData (GamePlayState)
    | IntroPage
    | ResearchCenter GameData
    | TripDestinationSelect GameData
    | TripEquipmentSelect GameData GameLocation [T.Text] Int
    | TripReview GameData GameLocation [T.Text]
    | TripProgress GameData TripState
    | GameExitState (Maybe GameData)
    | ComingSoon

data OverlayMenu = Overlay
    { bgXPos :: !Int
    , bgYPos :: !Int
    , bgWidth :: !Int
    , bgHeight :: !Int
    , bgColor :: !Color
    , overlayMenu :: !Menu
    }

-- Actions that can be done from the Menu
--  Start makes a new game area
--  Exit quits the game
--  Continue returns to the game area already started
--  Start takes you to start menu (currently no saving)
data MenuAction = MenuAction
    { menuOptionText :: !T.Text
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

data MenuOptions =
      SelOneListOpts OneActionListOptions
    | SelMultiListOpts MultiSelectListOptions
    -- todo options at given positions

selOneOpts :: Int -> Int -> Int -> Int -> [MenuAction] -> CursorType -> MenuOptions
selOneOpts x y s sp opts curs = SelOneListOpts $ OALOpts x y s sp opts curs

selMultOpts :: Int -> Int -> Int -> Int -> [SelectOption] -> ([T.Text] -> Int -> GamePlayState) -> ([T.Text] -> GamePlayState) -> Maybe GamePlayState -> MenuOptions
selMultOpts x y s sp opts up act back = SelMultiListOpts $ MSLOpts x y s sp opts up act back

getNextMenu :: Int -> MenuOptions -> GamePlayState
getNextMenu pos (SelOneListOpts opts) = menuNextState ((oalOpts opts) !! pos)
getNextMenu pos (SelMultiListOpts opts)
    | pos < len = (mslAction opts) selected pos
    | len == pos = (mslContinueAction opts) selected
    | otherwise =
        case mslBackActionM opts of
            Nothing -> (mslAction opts) selected pos
            Just back -> back
    where
        len = length (mslOpts opts)
        opts' = toggleMultiOption opts pos
        selected = selectKey <$> filter (\o -> selectSelected o) (mslOpts opts')

optionLength :: MenuOptions -> Int
optionLength (SelOneListOpts opts) = length $ oalOpts opts
optionLength (SelMultiListOpts opts) = 1 + (length $ mslOpts opts) + (if isJust (mslBackActionM opts) then 1 else 0)

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
    { texts :: ![TextDisplay]
    , imgs :: ![(Int, Int, TextureEntry)]
    , options :: !MenuOptions
    , currentPos :: !Int
    }


data CursorType = CursorPointer TextureEntry | CursorRect Color


type Barriers = RTree ()

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

