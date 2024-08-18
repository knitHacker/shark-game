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

import Data.Maybe (isJust)
import qualified Data.Text as T

import Debug.Trace


incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ _ opts p)
    | p < len - 1 = m { currentPos = p + 1 }
    | otherwise = m
    where
        len = optionLength opts

decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ _ 0) = m
decrementMenuCursor m@(Menu _ _ opt p)
    | optionLength opt >  0 = m { currentPos = p - 1 }
    | otherwise = m


updateGameStateInMenu :: Maybe OverlayMenu -> Menu -> GameConfigs -> InputState -> OutputHandles -> Maybe (Either GameView GamePlayState)
updateGameStateInMenu mM m cfgs inputs outs =
    case (esc, mM, selected, moveDirM) of
        (True, Just om, _, _) -> Just $ Left $ OverlayMenu om m
        (_, _, _, Just DUp) -> Just $ Left $ GameView mM $ decrementMenuCursor m
        (_, _, _, Just DDown) -> Just $ Left $ GameView mM $ incrementMenuCursor m
        (_, _, True, _) -> Just $ Right $ getNextMenu pos $ options m
        _ -> Nothing
    where
        pos = currentPos m
        moveDirM = if inputRepeating inputs then Nothing else inputDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        optLen = optionLength $ options m

updateGameStateInOverlay :: OverlayMenu -> Menu -> GameConfigs -> InputState -> OutputHandles -> Maybe (Either GameView GamePlayState)
updateGameStateInOverlay om@(Overlay _ _ _ _ _ topM) backM cfgs inputs outs =
    case (esc, selected, moveDirM) of
        (True, _, _) -> Just $ Left $ GameView (Just (om' topM)) backM
        (_, True, _) -> Just $ Right $ getNextMenu (currentPos topM) $ options topM
        (_, _, Just DUp) -> Just $ Left $ OverlayMenu (om' (decrementMenuCursor topM)) backM
        (_, _, Just DDown) -> Just $ Left $ OverlayMenu (om' (incrementMenuCursor topM)) backM
        _ -> Nothing
    where
        moveDirM = if inputRepeating inputs then Nothing else inputDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        om' newM = om { overlayMenu = newM }

withPause :: GamePlayState -> GameData -> Menu -> GameView
withPause gps gd m = GameView (Just (pauseMenu gps gd)) m

pauseMenu :: GamePlayState -> GameData -> OverlayMenu
pauseMenu gps gd = Overlay 20 20 150 200 Yellow menu
    where
        menu = Menu words [] menuOpt 0
        menuOpt = SelOneListOpts $ OALOpts 50 120 opts $ CursorRect Gray
        words = [ TextDisplay "Game Menu" 30 30 120 60 Black
                ]
        opts = [ MenuAction "Continue" gps
               , MenuAction "Main Menu" $ MainMenu gd
               , MenuAction "Exit" (GameExitState (Just gd))
               ]
