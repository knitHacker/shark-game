{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu
    ( exitMenuAction
    , updateGameStateInMenu
    , updateGameStateInOverlay
    , pauseMenu
    )
    where


import InputState
import Configs
import OutputHandles.Types
import GameState.Types

import SaveData


import Data.Maybe (isJust)

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

exitMenuAction :: GameConfigs -> InputState -> OutputHandles -> GameState
exitMenuAction _ _ _ = GameExiting Nothing

exitSaveAction :: GameData -> GameConfigs -> InputState -> OutputHandles -> GameState
exitSaveAction gd _ _ _ = GameExiting (Just gd)

updateGameStateInMenu :: Maybe OverlayMenu -> Menu -> GameConfigs -> InputState -> OutputHandles -> GameState
updateGameStateInMenu mM m cfgs inputs outs =
    case (esc, mM, selected, moveDirM) of
        (True, Just om, _, _) -> OverlayMenu om m
        (_, _, _, Just DUp) -> GameView mM $ decrementMenuCursor m
        (_, _, _, Just DDown) -> GameView mM $ incrementMenuCursor m
        (_, _, True, _) -> (activateOption pos (options m)) cfgs inputs outs
        _ -> GameView mM m
    where
        pos = currentPos m
        moveDirM = if inputRepeat inputs then Nothing else inputStateDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        optLen = optionLength $ options m

updateGameStateInOverlay :: OverlayMenu -> Menu -> GameConfigs -> InputState -> OutputHandles -> GameState
updateGameStateInOverlay om@(Overlay _ _ _ _ _ topM) backM cfgs inputs outs =
    case (esc, selected, moveDirM) of
        (True, _, _) -> GameView (Just (om' topM)) backM
        (_, True, _) -> (activateOption (currentPos topM) (options topM)) cfgs inputs outs
        (_, _, Just DUp) -> OverlayMenu (om' (decrementMenuCursor topM)) backM
        (_, _, Just DDown) -> OverlayMenu (om' (incrementMenuCursor topM)) backM
        _ -> OverlayMenu om backM
    where
        moveDirM = if inputRepeat inputs then Nothing else inputStateDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        om' newM = om { overlayMenu = newM }

pauseMenu :: Menu -> GameData -> OverlayMenu
pauseMenu m gd = Overlay 20 20 150 200 Yellow menu
    where
        menu = Menu words [] menuOpt 0
        menuOpt = SelOneListOpts $ OALOpts 50 120 opts $ CursorRect Gray
        words = [ TextDisplay "Game Menu" 30 30 120 60 Black
                ]
        opts = [ MenuAction "Continue" (\_ _ _ -> GameView (Just (pauseMenu m gd)) m)
               , MenuAction "Exit" (exitSaveAction gd)
               ]

