{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu
    ( updateGameStateInMenu
    , updateGameStateInOverlay
    , withPause
    )
    where


import InputState
import Configs
import OutputHandles.Types
import GameState.Types
import SaveData

import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T

import Debug.Trace


incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ opts p)
    | p < len - 1 = m { currentPos = p + 1 }
    | otherwise = m
    where
        len = optionLength opts

decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ 0) = m
decrementMenuCursor m@(Menu _ opt p)
    | optionLength opt >  0 = m { currentPos = p - 1 }
    | otherwise = m


updateGameStateInMenu :: Maybe OverlayMenu -> Menu -> InputState -> Maybe (Either GameView GamePlayState)
updateGameStateInMenu _ _ (InputState Nothing _) = Nothing
updateGameStateInMenu mM m inputs =
    case (esc, mM, selected, moveDirM) of
        (True, Just om, _, _) -> Just $ Left $ OverlayMenu om $ BasicMenu m
        (_, _, _, Just DUp) -> Just $ Left $ GameMenu mM $ decrementMenuCursor m
        (_, _, _, Just DDown) -> Just $ Left $ GameMenu mM $ incrementMenuCursor m
        (_, _, True, _) -> Right <$> (getNextMenu pos $ options m)
        _ -> Nothing
    where
        pos = currentPos m
        moveDirM = if inputRepeating inputs then Nothing else inputDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        optLen = optionLength $ options m

updateGameStateInOverlay :: OverlayMenu -> BasicView -> InputState -> Maybe (Either GameView GamePlayState)
updateGameStateInOverlay _ _ (InputState Nothing _) = Nothing
updateGameStateInOverlay om@(Overlay _ _ _ _ _ topM) backM inputs =
    case (esc, backM, selected, moveDirM) of
        (True, BasicMenu m, _, _) -> Just $ Left $ GameMenu (Just (om' topM)) m
        (True, BasicTimeoutView tov, _, _) -> Just $ Left $ GameTimeout (Just (om' topM)) tov
        (_, _, True, _) -> Right <$> (getNextMenu (currentPos topM) $ options topM)
        (_, _, _, Just DUp) -> Just $ Left $ OverlayMenu (om' (decrementMenuCursor topM)) backM
        (_, _, _, Just DDown) -> Just $ Left $ OverlayMenu (om' (incrementMenuCursor topM)) backM
        _ -> Nothing
    where
        moveDirM = if inputRepeating inputs then Nothing else inputDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        om' newM = om { overlayMenu = newM }

withPause :: GamePlayState -> GameData -> BasicView -> GameView
withPause gps gd (BasicMenu m) = GameMenu (Just (pauseMenu gps gd)) m
withPause gps gd (BasicTimeoutView m) = GameTimeout (Just (pauseMenu gps gd)) m

pauseMenu :: GamePlayState -> GameData -> OverlayMenu
pauseMenu gps gd = Overlay 20 20 200 200 Gray menu
    where
        menu = mkMenu words [] menuOpt 0
        menuOpt = SelOneListOpts $ OALOpts 50 120 5 15 opts $ CursorRect White
        words = [ TextDisplay "Game Menu" 30 30 10 Black
                ]
        opts = [ MenuAction "Continue" True gps
               , MenuAction "Main Menu" True $ MainMenu gd
               , MenuAction "Save & Exit" True (GameExitState (Just gd))
               ]
