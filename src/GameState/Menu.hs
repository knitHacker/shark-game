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

incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ _ (SelOneListOpts opts))
    | p < length (oalOpts opts) - 1 = menu'
    | otherwise = m
    where
        curs = oalCursor opts
        p = cursorPos curs
        menu' = m { options = SelOneListOpts (opts { oalCursor = curs { cursorPos = p + 1}})}
incrementMenuCursor m = m

decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ (SelOneListOpts (OALOpts _ _ _ (MenuCursor 0 _)))) = m
decrementMenuCursor m@(Menu _ _ (SelOneListOpts opts)) = menu'
    where
        curs = oalCursor opts
        p = cursorPos curs
        menu' = m { options = SelOneListOpts (opts { oalCursor = curs { cursorPos = p - 1}})}
decrementMenuCursor m = m

exitMenuAction :: GameConfigs -> InputState -> OutputHandles -> GameState
exitMenuAction _ _ _ = GameExiting Nothing

exitSaveAction :: GameData -> GameConfigs -> InputState -> OutputHandles -> GameState
exitSaveAction gd _ _ _ = GameExiting (Just gd)

updateGameStateInMenu :: Maybe OverlayMenu -> Menu -> GameConfigs -> InputState -> OutputHandles -> GameState
updateGameStateInMenu mM m cfgs inputs outs =
    case (esc, mM, selected, moveDirM) of
        (True, Just om, _, _) -> OverlayMenu om m
        (_, _, True, _) -> (activateOption (options m)) cfgs inputs outs
        (_, _, _, Just DUp) -> GameView mM $ decrementMenuCursor m
        (_, _, _, Just DDown) -> GameView mM $ incrementMenuCursor m
        _ -> GameView mM m
    where
        moveDirM = if inputRepeat inputs then Nothing else inputStateDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        -- need to use new key for selecting multi choice

updateGameStateInOverlay :: OverlayMenu -> Menu -> GameConfigs -> InputState -> OutputHandles -> GameState
updateGameStateInOverlay om@(Overlay _ _ _ _ _ topM) backM cfgs inputs outs =
    case (esc, selected, moveDirM) of
        (True, _, _) -> GameView (Just (om' topM)) backM
        (_, True, _) -> (activateOption (options topM)) cfgs inputs outs
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
        menu = Menu words [] menuOpt
        menuOpt = SelOneListOpts $ OALOpts 50 120 opts $ MenuCursor 0 $ CursorRect Gray
        words = [ TextDisplay "Game Menu" 30 30 120 60 Black
                ]
        opts = [ MenuAction "Continue" (\_ _ _ -> GameView (Just (pauseMenu m gd)) m)
               , MenuAction "Exit" (exitSaveAction gd)
               ]

