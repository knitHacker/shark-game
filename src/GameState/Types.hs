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
    , OptAction
    , CursorType(..)
    , OneActionListOptions(..)
    , MultiSelectListOptions(..)
    , SelectOption(..)
    , activateOption
    , selOneOpts
    , selMultOpts
    , optionLength
    , toggleMultiOption
    , reDraw
    , GameView(..)
    , noUpdate
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
import SaveData

instance Show Unique where
    show:: Unique -> String
    show = show . hashUnique

-- Top level game state
--  Game menu is a menu with different options
--  Game state is where character walks around
--  Game exiting is how tell top loop to quit
data GameState = GameState
    { gameView :: GameView
    , gameReDraw :: Bool
    , gameDrawCount :: Int
    }

data GameView =
      GameView !(Maybe OverlayMenu) !Menu
    | OverlayMenu !OverlayMenu !Menu
    | GameExiting !(Maybe GameData)

reDraw :: GameView -> GameState
reDraw gv = GameState gv True 0

noUpdate :: GameState -> GameState
noUpdate (GameState gv _ gdc)
    | gdc < 30 = GameState gv False (gdc + 1)
    | otherwise = reDraw gv

type OptAction = GameConfigs -> InputState -> OutputHandles -> GameView

data OverlayMenu = Overlay
    { bgXPos :: Int
    , bgYPos :: Int
    , bgWidth :: Int
    , bgHeight :: Int
    , bgColor :: Color
    , overlayMenu :: Menu
    }

-- Actions that can be done from the Menu
--  Start makes a new game area
--  Exit quits the game
--  Continue returns to the game area already started
--  Start takes you to start menu (currently no saving)
data MenuAction = MenuAction
    { menuOptionText :: T.Text
    , optAction :: OptAction
    }

data SelectOption = SelectOption
    { selectOptionText :: T.Text
    , selectKey :: T.Text
    , selectSelected :: Bool
    , changeable :: Bool
    }

data OneActionListOptions = OALOpts
    { oalXPos :: Int
    , oalYPos :: Int
    , oalOpts :: [MenuAction]
    , oalCursor :: CursorType
    }

data MultiSelectListOptions = MSLOpts
    { mslXPos :: Int
    , mslYPos :: Int
    , mslOpts :: [SelectOption]
    , mslAction :: [T.Text] -> Int -> OptAction
    , mslContinueAction :: [T.Text] -> OptAction
    , mslBackActionM :: Maybe OptAction
    }

data MenuOptions =
      SelOneListOpts OneActionListOptions
    | SelMultiListOpts MultiSelectListOptions
    -- todo options at given positions

selOneOpts :: Int -> Int -> [MenuAction] -> CursorType -> MenuOptions
selOneOpts x y opts curs = SelOneListOpts $ OALOpts x y opts curs

selMultOpts :: Int -> Int -> [SelectOption] -> ([T.Text] -> Int -> OptAction) -> ([T.Text] -> OptAction) -> Maybe OptAction -> MenuOptions
selMultOpts x y opts up act back = SelMultiListOpts $ MSLOpts x y opts up act back

activateOption :: Int -> MenuOptions -> OptAction
activateOption pos (SelOneListOpts opts) = optAction ((oalOpts opts) !! pos)
activateOption pos (SelMultiListOpts opts)
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
    { texts :: [TextDisplay]
    , imgs :: [(Int, Int, TextureEntry)]
    , options :: MenuOptions
    , currentPos :: Int
    }


data CursorType = CursorPointer TextureEntry | CursorRect Color


type Barriers = RTree ()

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

