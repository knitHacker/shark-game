{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification#-}


module GameState
    ( initGameState
    , stepGameState
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
import Graphics.NewTypes
import Graphics.Menu
import Graphics.Animation
import Graphics.TextUtil
import Graphics.Asset

import qualified Data.Text as T
import Data.Int (Int64)

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

import Debug.Trace
import Data.Maybe (catMaybes)

initGameState :: GameConfigs -> InputState -> Graphics -> GameStateNew
initGameState cfgs inputs gr = transition (initSplash inputs) cfgs gr

stepGameState :: GameStep -> GameStateNew -> GameConfigs -> Graphics -> GameStateNew
stepGameState step gsn cfgs gr =
    case step of
        NoChange -> gsn
        ResizeWindow -> gsn { gView = resizeGameView (gameStateE gsn) gr (gView gsn) }
        (InputUpdate (AnyGamePlayState nGps)) -> update nGps gsn cfgs gr
        (Transition (AnyGamePlayState gps)) -> transition gps cfgs gr
        (StepAnimation currTS anims) ->
            let gv' = foldl (applyAnimationState currTS gr) (gView gsn) anims
            in case gameStateE gsn of
                        (AnyGamePlayState gpse) -> gsn { gameStateE = AnyGamePlayState (updateAnims gpse anims), gView = gv' }

updateGameView :: Graphics -> InputState -> GameView -> Maybe (Either GameView GamePlayState)
updateGameView gr inputs gv@(GameView vl oM tL mM) = if null outs then Nothing else Just (head outs)
    where
        viewUp = viewScroll vl >>= updateGameViewScroll inputs
        overlayUp = oM >>= updateGameOverlay inputs
        timeoutUp = updateGameTimeout gr vl inputs tL
        menuUp = mM >>= updateGameMenu inputs
        out = case viewUp of
            Nothing -> Nothing
            jvs -> Just $ Left $ gv { viewLayer = (viewLayer gv) { viewScroll = jvs } }
        out2 = case overlayUp of
                Just (Left ov) -> Just $ Left $ gv { viewOverlay = Just ov }
                Just (Right gps) -> Just $ Right gps
                _ -> Nothing
        -- If overlay is open don't timeout
        out3 = case (oM, timeoutUp) of
                (Just (OverlayView True _ _), _) -> Nothing  -- Overlay is active, don't process timeouts
                (_, Just (Right gps)) -> Just $ Right gps
                (_, Just (Left (td, v'))) -> Just $ Left $ gv { viewTimeouts = td, viewLayer = v' }
                _ -> Nothing
        out4 = case menuUp of
                Just (Left m) -> Just $ Left $ gv { viewMenu = Just m }
                Just (Right gps) -> Just $ Right gps
                _ -> Nothing
        outs = catMaybes [out, out2, out4, out3]

updateGameViewScroll :: InputState -> ViewScroll GamePlayState -> Maybe (ViewScroll GamePlayState)
updateGameViewScroll i vs = case mouseInputs i of
    Just mi -> Just $ scrollView vs (scrollAmt mi)
    Nothing -> Nothing

updateGameTimeout :: Graphics -> View GamePlayState -> InputState -> [TimeoutData GamePlayState] -> Maybe (Either ([TimeoutData GamePlayState], View GamePlayState) GamePlayState)
updateGameTimeout _ _ _ [] = Nothing
updateGameTimeout gr vO i@(InputState _ _ _ ts) timeouts =
    case outs of
        Left (tds, v') -> Just $ Left (tds, v')
        Right gps -> Just $ Right gps
    where
        outs = foldr checkTimeout (Left ([], vO)) timeouts
        checkTimeout _ (Right gps) = Right gps
        checkTimeout td@(TimeoutData lTO toLen ta) (Left (tds, v'))
            | ts - lTO > toLen = case ta of
                TimeoutNext gps -> Right gps
                TimeoutAnimation animData ->
                    let (ad, v'') = updateAnimation gr v' animData
                    in Left (tds++[td { lastTimeout = ts, timeoutAction = TimeoutAnimation ad }], v'')
            | otherwise = Left (tds++[td], v')

updateGameOverlay :: InputState -> OverlayView GamePlayState -> Maybe (Either (OverlayView GamePlayState) GamePlayState)
updateGameOverlay i ov@(OverlayView active _ om)
    | inputRepeating i = Nothing
    | escapePressed i = Just $ Left $ ov { isActive = not active }
    | not active = Nothing
    | otherwise = case updateOverlayMenu i om of
                    Just (Left om') -> Just $ Left $ ov { overlayMenu = om' }
                    Just (Right gps) -> Just $ Right gps
                    _ -> Nothing

updateOverlayMenu :: InputState -> OverlayMenu GamePlayState -> Maybe (Either (OverlayMenu GamePlayState) GamePlayState)
updateOverlayMenu inputs om@(Overlay _ _ _ _ _ md)
    | enterJustPressed inputs = Right <$> getNextOption md
    | inputRepeating inputs = Nothing
    | inputDirection inputs == Just DDown = Just $ Left $ om { overlayData = incrementMenuOpt md }
    | inputDirection inputs == Just DUp = Just $ Left $ om { overlayData = decrementMenuOpt md }
    | otherwise = Nothing


updateGameMenu :: InputState -> Menu GamePlayState -> Maybe (Either (Menu GamePlayState) GamePlayState)
updateGameMenu inputs m
    | selected = Right <$> getNextMenu m
    | backSelected = Right <$> getBackOption m
    | inputRepeating inputs = Nothing
    | inputDirection inputs == Just DDown = Just $ incrementMenuCursor m
    | inputDirection inputs == Just DUp = Just $ decrementMenuCursor m
    | otherwise = Nothing
    where
        selected = enterJustPressed inputs
        backSelected = backJustPressed inputs


withPause :: Graphics -> GamePlayState -> GameData -> GameView -> GameView
withPause gr gps gd gv = gv { viewOverlay = Just $ pauseMenu gr gps gd }

pauseMenu :: Graphics -> GamePlayState -> GameData -> OverlayView GamePlayState
pauseMenu gr gps gd = OverlayView False (textView words) (Overlay overlayX overlayY 750 600 DarkBlue menuOpt)
    where
        overlayX = midStartX gr 750
        overlayY = midStartY gr 600
        menuOpt = MenuData (SelOneListOpts $ OALOpts opts Nothing Nothing (CursorRect White)) (BlockDrawInfo (overlayX + 150) (overlayY + 300) 4 15) 0
        words = [ TextDisplay "Game Menu" (fromIntegral (overlayX + 50)) (fromIntegral (overlayY + 50)) 8 White Nothing
                ]
        opts = [ MenuAction "Continue" Nothing $ Just gps
               , MenuAction "Main Menu" Nothing $ Just $ MainMenu $ Just gd
               , MenuAction "Save & Exit" Nothing $ Just (GameExitState (Just gd))
               ]
