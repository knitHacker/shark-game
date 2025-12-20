module GameState.Types
    ( BlockDrawInfo(..)
    , CursorType(..)
    , GamePlayState(..)
    , GameState(..)
    , GameDrawInfo(..)
    , GameStateRead(..)
    , GameView(..)
    , OverlayView(..)
    , GameMenu(..)
    ) where

import qualified Data.Map.Strict as M
import Data.Int ( Int64 )
import qualified Data.Text as T
import Data.Maybe (isJust)

import OutputHandles.Types
import Graphics.Types
import SaveData
import Shark.Types
import Util

data GameState = GameState
    { gameGraphics :: !Graphics
    , gameLastState :: !GamePlayState
    , gameView :: !GameDrawInfo
    , gameLastDraw :: !(Maybe ToRender)
    }

data OverlayView a = OverlayView
    { isActive :: Bool
    , overlayView :: View a
    , overlayMenu :: OverlayMenu a
    }

data GameDrawInfo = GameViewInfo GameView | GameExiting

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

data GamePlayState =
      MainMenu (Maybe GameData)
    | PauseMenu GameData GamePlayState
    | IntroWelcome
    | IntroMission GameData
    | IntroBoat GameData
    | IntroEquipment GameData
    | IntroResearch GameData
    | IntroFunds GameData
    | IntroEnd GameData
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


-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

