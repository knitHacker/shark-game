{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification#-}


module GameState
    ( initGameState
    , stepGameState
    , doTransition
    ) where

import Control.Monad ()
import InputState
import GameState.Types
import OutputHandles.Types
import GameState.Menu.GameMenus
import GameState.Menu.TripMenus
import GameState.Menu.DataReviewMenu
import GameState.Menu.LabMenus
import SaveData
import Configs
import Graphics
import Graphics.Types
import Graphics.TextUtil
import Graphics.Asset

import qualified Data.Text as T
import Data.Int (Int64)

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

import Debug.Trace
import Data.Maybe (catMaybes)

doTransition :: GamePlayStateE a => a -> GameConfigs -> Graphics -> GameState
doTransition gps cfgs gr = GameState (AnyGamePlayState (updateAnims gps anims)) gv
    where (gv, anims) = transition gps cfgs gr

initGameState :: GameConfigs -> InputState -> Graphics -> GameState
initGameState cfgs inputs gr = doTransition (initSplash inputs) cfgs gr

stepGameState :: GameStep -> GameState -> GameConfigs -> Graphics -> GameState
stepGameState step gsn cfgs gr =
    case step of
        NoChange -> gsn
        ResizeWindow -> gsn { gView = resizeGameView (gameStateE gsn) gr (gView gsn) }
        (InputUpdate (AnyGamePlayState nGps)) -> update nGps (gView gsn) cfgs gr
        (Transition (AnyGamePlayState gps)) -> doTransition gps cfgs gr
        (StepAnimation currTS anims) ->
            let gv' = foldl (applyAnimationState currTS gr) (gView gsn) anims
            in case gameStateE gsn of
                        (AnyGamePlayState gpse) -> gsn { gameStateE = AnyGamePlayState (updateAnims gpse anims), gView = gv' }
        (TopTransition sec gd) -> topTransition sec gd cfgs gr


topTransition :: GameSection -> GameData -> GameConfigs -> Graphics -> GameState
topTransition sec gd cfgs gr =
    case sec of
        TopMainMenu -> doTransition (initMainMenu (Just gd)) cfgs gr
        ResearchCenterMenu -> doTransition (initResearchCenter gd) cfgs gr
        TripMenus -> doTransition (initTripMapState gd) cfgs gr
        LabMenus -> doTransition (initLabTopState gd) cfgs gr
        ReviewMenus -> doTransition (initTopReviewState gd) cfgs gr
