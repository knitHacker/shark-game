module GameState.Types
    ( BlockDrawInfo(..)
    , CursorType(..)
    , GamePlayState(..)
    , GameState(..)
    , GameStateRead(..)
    , GameView(..)
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
    , gameView :: !GameView
    , gameLastDraw :: !(Maybe ToRender)
    }

data GameView =
      GameMenu !(Maybe (OverlayMenu GamePlayState)) !(Menu GamePlayState)
    | OverlayMenu !(OverlayMenu GamePlayState) !(BasicView GamePlayState)
    | GameTimeout !(Maybe (OverlayMenu GamePlayState)) !(TimeoutView GamePlayState)
    | GameExiting


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
    | LabManagement GameData
    | FleetManagement GameData
    | ComingSoon



-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState

