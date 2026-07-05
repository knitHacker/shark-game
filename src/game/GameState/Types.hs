{-# LANGUAGE ExistentialQuantification#-}

module GameState.Types
    ( GameStateRead(..)
    , GameStateStep(..)
    , GameStep(..)
    , Action(..)
    , AnyGamePlayState(..)
    , GamePlayStateE(..)
    , GameSection(..)
    , GameState(..)
    , SimpleStateInfo(..)
    , MenuStateInfo(..)
    , PauseOpt(..)
    ) where

import Data.Int ( Int64 )


import OutputHandles.Types
import Graphics.Types
import Configs
import InputState
import SaveData
import Data.Typeable (Typeable, typeOf)


data GameStep =
      NoChange
    | ResizeWindow -- pass in resize here?
    | StepAnimation Int64 [AnimationState]
    | InputUpdate AnyGamePlayState -- Type for what input action? up / down?
    | Transition AnyGamePlayState
    | TopTransition GameSection GameData

data Action =
      Step GameStep
    | LoadSave FilePath (GameData -> GameStep)
    | SaveData GameData GameStep
    | Exit (Maybe GameData)
    | SaveList ([FilePath] -> GameStep)
    | NewGame (GameData -> GameStep)

data GameSection =
      TopMainMenu
    | ResearchCenterMenu
    | TripMenus
    | ReviewMenus
    | LabMenus
    deriving (Show, Eq)

data PauseOpt = ContinuePause | MainMenuPause | ExitPause
               deriving (Show, Ord, Bounded, Enum, Eq)

data SimpleStateInfo = SimpleInfo
    { gamedata :: GameData
    , pauseSelM :: Maybe PauseOpt
    }

data MenuStateInfo a = MenuInfo
    { menuSel :: a
    , stateInfo :: SimpleStateInfo
    }

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

class Monad m => GameStateStep m where
    getAction :: m Action
    executeAction :: Action -> m (Maybe GameStep)
    stepGame :: GameStep -> m ToRender


class Typeable a => GamePlayStateE a where
    think :: a -> GameConfigs -> InputState -> Action

    transition :: a -> GameConfigs -> Graphics -> (GView, [AnimationState])
    transition _ _ _ = (GView mempty mempty Nothing, [])

    update :: a -> GView -> GameConfigs -> Graphics -> GameState
    update gps _ cfgs gr = GameState (AnyGamePlayState gps) $ fst $ transition gps cfgs gr

    updateAnims :: a -> [AnimationState] -> a
    updateAnims a _ = a

    {-# MINIMAL think #-}

data AnyGamePlayState = forall a. GamePlayStateE a => AnyGamePlayState
    { gameAState :: a
    }

instance Show AnyGamePlayState where
    show (AnyGamePlayState gps) = show (typeOf gps)

data GameState = GameState
    { gameStateE :: AnyGamePlayState
    , gView :: GView
    }
