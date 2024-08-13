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
--    , GameView(..)
    , Menu(..)
    , MenuAction(..)
    , MenuCursor(..)
    , MenuOptions(..)
    , OptAction
    , CursorType(..)
    ) where

import Control.Lens
import Control.Monad ()
import Control.Monad.IO.Class ()
import qualified Data.Map.Strict as M
import Data.Unique ( Unique, hashUnique )
import Data.Word ( Word32 )
import qualified Data.Text as T
import InputState

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

import Utils ()

instance Show Unique where
    show:: Unique -> String
    show = show . hashUnique

--data GameView =
--      GameMenu Menu
--    | Other

-- Top level game state
--  Game menu is a menu with different options
--  Game state is where character walks around
--  Game exiting is how tell top loop to quit
data GameState =
      GameView (Maybe Menu) Menu -- GameView
    | OverlayMenu Menu Menu -- GameView
    | GameExiting (Maybe GameData)

type OptAction = GameConfigs -> InputState -> OutputHandles -> GameState

-- Actions that can be done from the Menu
--  Start makes a new game area
--  Exit quits the game
--  Continue returns to the game area already started
--  Start takes you to start menu (currently no saving)
data MenuAction = MenuAction
    { menuOptionText :: T.Text
    , menuAction :: OptAction
    }

data MenuOptions = MenuOpts
    { optionXPos :: Int
    , optionYPos :: Int
    , menuOpts :: [MenuAction]
    }

-- Menu game state
--  Texts are the text to show including where to display
--  Options for actions from this menu
--  Cursor is the current option that is being pointed to
data Menu = Menu
    { texts :: [TextDisplay]
    , options :: MenuOptions
    , cursor :: MenuCursor
    }

data CursorType = CursorPointer TextureEntry | CursorRect Color

-- Menu cursor state
--  which option index the cursor is pointing to
--  the texture of the cursor
data MenuCursor = MenuCursor
    { cursorPos :: Int
    , cursorType :: CursorType
    }

type Barriers = RTree ()

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

