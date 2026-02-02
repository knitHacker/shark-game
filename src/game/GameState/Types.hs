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
    , TransitionBehavior(..)
    , mergeGameViews
    , mergeGameDrawInfo
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
import Data.IntMap.Merge.Lazy (merge)

data TransitionBehavior = AllowTransitions | BlockTransitions
    deriving (Eq, Show)

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

mergeGameDrawInfo :: GameDrawInfo -> GameDrawInfo -> GameDrawInfo
mergeGameDrawInfo (GameViewInfo gvInputUpdates) (GameViewInfo gvNewDraw) =
    GameViewInfo $ mergeGameViews gvInputUpdates gvNewDraw
mergeGameDrawInfo _ gvNewDraw = gvNewDraw

mergeGameViews :: GameView -> GameView -> GameView
mergeGameViews gvInputUpdates gvNewDraw = GameView
    { viewLayer = updateView (viewLayer gvInputUpdates) (viewLayer gvNewDraw)
    , viewOverlay = case (viewOverlay gvInputUpdates, viewOverlay gvNewDraw) of
        (Just ov1, Just ov2) -> Just $ OverlayView
            { isActive = isActive ov2
            , overlayView = updateView (overlayView ov1) (overlayView ov2)
            , overlayMenu = mergeOverlayMenu (overlayMenu ov1) (overlayMenu ov2)
            }
        (_, ov2M) -> ov2M
    , viewTimeouts = updateTimeoutData (viewTimeouts gvInputUpdates) (viewTimeouts gvNewDraw)
    , viewMenu = case (viewMenu gvInputUpdates, viewMenu gvNewDraw) of
        (Just m1, Just m2) -> Just $ updateMenu m1 m2
        (_, m2M) -> m2M
    }

data GamePlayState =
      MainMenu (Maybe GameData)
    | PauseMenu GameData GamePlayState
    | IntroWelcome (Maybe GameData)
    | IntroMission GameData
    | IntroBoat GameData
    | IntroEquipment GameData
    | IntroResearch GameData
    | IntroFunds GameData
    | IntroEnd GameData
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


-- Class for reading game state from the top level monad
class Monad m => GameStateRead m where
    readGameState :: m GameState
