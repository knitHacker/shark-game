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
incrementMenuCursor m@(Menu _ opts c@(MenuCursor p _))
    | p < length (menuOpts opts) - 1 = m { cursor = c { cursorPos = p + 1 }}
    | otherwise = m


decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ c@(MenuCursor 0 _)) = m
decrementMenuCursor m@(Menu _ _ c@(MenuCursor p _)) = m { cursor = c { cursorPos = p - 1 }}

exitMenuAction :: GameConfigs -> InputState -> OutputHandles -> GameState
exitMenuAction _ _ _ = GameExiting

exitSaveAction :: GameData -> GameConfigs -> InputState -> OutputHandles -> GameState

updateGameStateInMenu :: Maybe Menu -> Menu -> GameConfigs -> InputState -> OutputHandles -> GameState
updateGameStateInMenu mM m cfgs inputs outs =
    case (esc, mM, selected, moveDirM) of
        (True, Just om, _, _) -> OverlayMenu om m
        (_, _, True, _) ->
            let action = menuOpts (options m) !! curPos
            in (menuAction action) cfgs inputs outs
        (_, _, _, Just DUp) -> GameView mM $ decrementMenuCursor m
        (_, _, _, Just DDown) -> GameView mM $ incrementMenuCursor m
        _ -> GameView mM m
    where
        moveDirM = if inputRepeat inputs then Nothing else inputStateDirection inputs
        curPos = cursorPos $ cursor m
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs

updateGameStateInOverlay :: Menu -> Menu -> GameConfigs -> InputState -> OutputHandles -> GameState
updateGameStateInOverlay topM backM cfgs inputs outs =
    case (esc, selected, moveDirM) of
        (True, _, _) -> GameView (Just topM) backM
        (_, True, _) ->
            let action = menuOpts (options topM) !! curPos
            in (menuAction action) cfgs inputs outs
        (_, _, Just DUp) -> OverlayMenu (decrementMenuCursor topM) backM
        (_, _, Just DDown) -> OverlayMenu (incrementMenuCursor topM) backM
        _ -> OverlayMenu topM backM
    where
        moveDirM = if inputRepeat inputs then Nothing else inputStateDirection inputs
        curPos = cursorPos $ cursor topM
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs

pauseMenu :: Menu -> GameData -> Menu
pauseMenu m gd = Menu words (MenuOpts 50 120 opts) $ MenuCursor 0 $ CursorRect Gray
    where
        words = [ TextDisplay "Game Menu" 30 30 120 60 Black
                ]
        opts = [ MenuAction "Continue" (\_ _ _ -> GameView (Just (pauseMenu m gd)) m)
               , MenuAction "Exit" (\_ _ _ -> GameExiting)
               ]


