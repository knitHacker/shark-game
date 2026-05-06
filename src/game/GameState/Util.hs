{-# LANGUAGE OverloadedStrings #-}

module GameState.Util
    (shouldAnimationStep
    , getAnimationStep
    , pauseOverlay
    , getPauseEnterAction
    , getPauseMoveAction
    , PauseOpt(..)
    , simpleUpdate
    , updateMenuHighlight
    , updateMenuCursor
    , withPauseUpdate
    , updateOverlayHighlight
    , openPauseMenu
    , stepInputUpdate
    ) where

import qualified Data.Map.Strict as M

import InputState
import Graphics.NewTypes
import Graphics.Types
import Graphics.Asset
import GameState.Types
import Graphics.TextUtil
import OutputHandles.Types
import SaveData

simpleUpdate :: GamePlayStateE a => a -> (GView -> GView) -> GameStateNew -> GameStateNew
simpleUpdate gps f gsn = gsn { gameStateE = AnyGamePlayState gps, gView = f (gView gsn) }

updateMenuHighlight :: Int -> Color -> GView -> GView
updateMenuHighlight idx c gv = gv { menuAsset = (\ma -> changeHighlight ma idx c) <$> menuAsset gv }

updateMenuCursor :: Int -> Image -> Double -> GView -> GView
updateMenuCursor idx img scale gv = gv { menuAsset = (\ma -> changeCursor ma idx img scale) <$> menuAsset gv }

shouldAnimationStep :: InputState -> AnimationState -> Bool
shouldAnimationStep inputs as@(AnimState lastTs interval _ _) = timestamp inputs - lastTs > fromIntegral interval

getAnimationStep :: InputState -> [AnimationState] -> GameStep
getAnimationStep inputs animStates = if not shouldStep then NoChange else StepAnimation ts updatedAnims
    where
        ts = timestamp inputs
        (updatedAnims, shouldStep) = foldl foldFn ([], False) animStates
        foldFn (anims, should) animState
            | shouldAnimationStep inputs animState = (anims ++ [animState { lastAnimTS = ts }], True)
            | otherwise = (anims ++ [animState], should)

data PauseOpt = ContinuePause | MainMenuPause | ExitPause
               deriving (Show, Ord, Bounded, Enum, Eq)

pauseOverlay :: Graphics -> Maybe PauseOpt -> Overlay
pauseOverlay gr mPSel = AOverlay assets (Just overMenu) (ox gr) (oy gr) ow oh DarkBlue $ Just oRsz
    where
        ow = 750
        oh = 600
        ox gr' = midStartX gr' ow
        oy gr' = midStartY gr' oh
        titleX gr' = midTextStart gr' "Pause" (fromIntegral 8)
        titleRsz titleA gr' = titleA { assetX = titleX gr', assetY = oy gr' + 50 }
        assets = M.fromList [(0, Asset (AssetText "Pause" White 8) (titleX gr) (oy gr + 50) 2 True (Just titleRsz))]
        mRsz mAss gr' = mAss { menuXBase = ox gr' + 150, menuYBase = oy gr' + 300 }
        overMenu = MenuAsset ((ox gr) + 150) ((oy gr) + 300) 2 (Just mRsz) False 3 mItems
        oRsz ov gr' = ov { oAssets = resizeAssets (oAssets ov) gr', oMenu = resizeMenu (oMenu ov) gr', overlayX = ox gr', overlayY = oy gr' }
        getHl sel = case mPSel of
                        Nothing -> if sel == ContinuePause then Just White else Nothing
                        (Just psel) -> if sel == psel then Just White else Nothing
        mItems = [ MenuItem "Continue" Blue 4 0 0 (getHl ContinuePause) Nothing
                 , MenuItem "Main Menu" Blue 4 0 0 (getHl MainMenuPause) Nothing
                 , MenuItem "Save & Exit" Blue 4 0 0 (getHl ExitPause) Nothing
                 ]

getPauseEnterAction :: PauseOpt -> GameData -> AnyGamePlayState -> Action
getPauseEnterAction ContinuePause _ closedGPS = Step $ InputUpdate $ closedGPS
getPauseEnterAction MainMenuPause gd _ = Step $ TopTransition TopMainMenu gd
getPauseEnterAction ExitPause gd _ = Exit $ Just gd

getPauseMoveAction :: Direction -> PauseOpt -> (PauseOpt -> AnyGamePlayState) -> Action
getPauseMoveAction dir po update =
    case (dir, po) of
        (DUp, ContinuePause) -> Step NoChange
        (DUp, pSel) -> Step $ InputUpdate $ update $ pred pSel
        (DDown, ExitPause) -> Step NoChange
        (DDown, pSel) -> Step $ InputUpdate $ update $ succ pSel
        _ -> Step NoChange

withPauseUpdate :: GamePlayStateE a => a -> Maybe PauseOpt -> (GView -> GView) -> GameStateNew -> GameStateNew
withPauseUpdate gps Nothing extraF gsn = simpleUpdate gps (extraF . \gv -> gv { activeOverlays = filter (/= 0) (activeOverlays gv) }) gsn
withPauseUpdate gps (Just pSel) extraF gsn = simpleUpdate gps (extraF . updateOverlayHighlight 0 (fromEnum pSel) White) gsn

updateOverlayHighlight :: Int -> Int -> Color -> GView -> GView
updateOverlayHighlight overlayIdx menuIdx c gv = gv { overlays = M.adjust updateOv overlayIdx (overlays gv), activeOverlays = [overlayIdx] }
    where
        updateOv ov = ov { oMenu = (\ma -> changeHighlight ma menuIdx c) <$> oMenu ov }

stepInputUpdate :: GamePlayStateE a => a -> Action
stepInputUpdate gps = Step $ InputUpdate $ AnyGamePlayState gps

openPauseMenu :: GamePlayStateE a => (Maybe PauseOpt -> a) -> Action
openPauseMenu stateUp = Step $ InputUpdate $ AnyGamePlayState $ stateUp $ Just minBound
