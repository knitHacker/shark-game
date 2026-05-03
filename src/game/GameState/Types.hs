{-# LANGUAGE ExistentialQuantification#-}

module GameState.Types
    ( BlockDrawInfo(..)
    , CursorType(..)
    , GamePlayState(..)
    , GameState(..)
    , GameStateRead(..)
    , GameView(..)
    , OverlayView(..)
    , GameMenu(..)
    , GameStateStep(..)
    , GameStep(..)
    , Action(..)
    , AnyGamePlayState(..)
    , GamePlayStateE(..)
    , GameSection(..)
    , GameStateNew(..)
    ) where

import qualified Data.Map.Strict as M
import Data.Int ( Int64 )
import qualified Data.Text as T
import Data.Maybe (isJust)


import OutputHandles.Types
import Graphics.Types
import Graphics.NewTypes
import Graphics.Asset
import Configs
import InputState
import SaveData
import Shark.Types
import Util
import Data.IntMap.Merge.Lazy (merge)


data GameState = GameState
    { gameState :: !GamePlayState
    , gameView :: !GameView
    , exiting :: !Bool
    }

data OverlayView a = OverlayView
    { isActive :: Bool
    , overlayView :: View a
    , overlayMenu :: OverlayMenu a
    }


data GameMenu = GameMenu
    { gameViewLayer :: View GamePlayState
    , gameMenuLayer :: Menu GamePlayState
    }

data GameView = GameView
    { viewLayer :: View GamePlayState
    , viewOverlay :: Maybe (OverlayView GamePlayState)
    , viewTimeouts :: [TimeoutData GamePlayState]
    , viewMenu :: Maybe (Menu GamePlayState)
    }

data GameStep =
      NoChange
    | ResizeWindow -- pass in resize here?
    | StepAnimation -- Todo: list of animations that need to be updated []
    | InputUpdate AnyGamePlayState -- Type for what input action? up / down?
    | Transition AnyGamePlayState

data Action =
      Step GameStep
    | LoadSave FilePath (GameData -> GameStep)
    | SaveData GameData GameStep
    | Exit (Maybe GameData)
    | SaveList ([FilePath] -> GameStep)
    | NewGame (GameData -> GameStep)

data GamePlayState =
      SplashScreen Int64
    | MainMenu (Maybe GameData)
    | PauseMenu GameData GamePlayState
    | ResearchCenter GameData
    | TripDestinationSelect GameData Int
    | TripEquipmentSelect GameData (Int, T.Text) [T.Text] Int
    | TripReview GameData (Int, T.Text) [T.Text]
    | TripProgress GameData TripState
    | GameExitState (Maybe GameData)
    | SharkFound GameData (Maybe SharkFind) TripState
    | TripResults GameData TripState
    | DataReviewTop GameData
    | SharkReviewTop GameData (Maybe T.Text)
    | SharkReview GameData (DataEntry T.Text SharkInfo)
    | ResearchReviewTop GameData
    | OpenResearchMenu GameData
    | CompletedResearchMenu GameData
    | InvestigateResearchMenu GameData (DataEntry T.Text ResearchData)
    | AwardGrantMenu GameData (DataEntry T.Text ResearchData)
    | CompletedResearchReviewMenu GameData (DataEntry T.Text ResearchData)
    | LabManagement GameData
    | FundraiserTop GameData
    | FleetManagement GameData
    | EquipmentManagement GameData
    | EquipmentStore (Maybe (T.Text, Int, GameData)) GameData
    | BoatStore (Maybe (T.Text, Int, GameData)) GameData
    | ChooseBoat GameData
    | ViewDonors GameData
    | NewFundraiser GameData
    deriving (Eq, Show)

data GameSection =
      StartGame
    | MainMenus
    | TripMenus
    | ReviewMenus
    | LabMenus

-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameStateNew

class Monad m => GameStateStep m where
    getAction :: m Action
    executeAction :: Action -> m (Maybe GameStep)
    stepGame :: GameStep -> m ToRender


class GamePlayStateE a where
    think :: a -> GameConfigs -> InputState -> Action

    transition :: a -> GameConfigs -> Graphics -> GameStateNew
    transition gps _ _ = GameStateNew (AnyGamePlayState gps) (GView mempty mempty [] Nothing)

    update :: a -> GameStateNew -> GameConfigs -> Graphics -> GameStateNew
    update gps _ cfgs gr = transition gps cfgs gr

    {-# MINIMAL think #-}

data AnyGamePlayState = forall a. GamePlayStateE a => AnyGamePlayState
    { gameAState :: a
    }

data GameStateNew = GameStateNew
    { gameStateE :: AnyGamePlayState
    , gView :: GView
    }
