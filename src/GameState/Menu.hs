module GameState.Menu
    ( exitMenuAction
    , updateGameStateInMenu
    )
    where


import InputState
import Configs
import OutputHandles.Types
import GameState.Types

import SaveData


incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ _ opts c@(MenuCursor p _))
    | p < length (menuOpts opts) - 1 = m { cursor = c { cursorPos = p + 1 }}
    | otherwise = m


decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ _ _ c@(MenuCursor 0 _)) = m
decrementMenuCursor m@(Menu _ _ _ c@(MenuCursor p _)) = m { cursor = c { cursorPos = p - 1 }}

exitMenuAction :: GameConfigs -> InputState -> OutputHandles -> GameState
exitMenuAction _ _ _ = GameExiting

updateGameStateInMenu :: Menu -> GameData -> GameConfigs -> InputState -> OutputHandles -> IO GameState
updateGameStateInMenu m gd cfgs inputs outs =
    case (shouldPause, selected, moveDirM) of
        (True, _, _) -> undefined -- todo: open overlay pause
        (_, True, _) ->
            let action = menuOpts (options m) !! curPos
            in return $ (menuAction action) cfgs inputs outs
        (_, _, Just DUp) -> return $ GameMenu (decrementMenuCursor m) gd
        (_, _, Just DDown) -> return $ GameMenu (incrementMenuCursor m) gd
        _ -> return $ GameMenu m gd
    where
        moveDirM = if inputRepeat inputs then Nothing else inputStateDirection inputs
        curPos = cursorPos $ cursor m
        selected = enterJustPressed inputs
        shouldPause = escapeJustPressed inputs && canPause m
