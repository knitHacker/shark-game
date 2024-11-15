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
import GameState.Menu.GameMenus (mainMenu)

-- TODO: Learn about the lens library
incrementMenuCursor :: Menu -> Menu
incrementMenuCursor m@(Menu _ mo@(MenuOptions (ScrollListOpts sl@(SLOpts _ fx (Scroll mx off))) _ p))
    | p >= len - 1 = m
    | p + off < end - 1 || off >= scrollLen - end = m { options = mo { cursorPosition = p + 1 } }
    | otherwise = m { options = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off + 1) })
                                   , cursorPosition = p + 1 }
                    }
    where
        len = optionLength mo
        fxLen = length fx
        scrollLen = len - fxLen
        end = min mx len

incrementMenuCursor m@(Menu _ mo@(MenuOptions _ _ p))
    | p >= optionLength mo - 1 = m
    | otherwise = m { options = mo { cursorPosition = p + 1 } }

decrementMenuCursor :: Menu -> Menu
decrementMenuCursor m@(Menu _ mo@(MenuOptions (ScrollListOpts sl@(SLOpts _ _ (Scroll mx off))) _ p))
    | p == 0 = m
    | p > off = m { options = mo { cursorPosition = p - 1 } }
    | otherwise = m { options = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off - 1) })
                                   , cursorPosition = p - 1 }
                    }
    where
        opts = options m
decrementMenuCursor m@(Menu _ mo@(MenuOptions _ _ p))
    | p == 0 = m
    | otherwise = m { options = mo { cursorPosition = p - 1 } }

updateGameStateInMenu :: Maybe OverlayMenu -> Menu -> InputState -> Maybe (Either GameView GamePlayState)
updateGameStateInMenu _ _ (InputState Nothing _) = Nothing
updateGameStateInMenu mM m inputs =
    case (esc, mM, selected, moveDirM) of
        (True, Just om, _, _) -> Just $ Left $ OverlayMenu om $ BasicMenu m
        (_, _, _, Just DUp) -> Just $ Left $ GameMenu mM $ decrementMenuCursor m
        (_, _, _, Just DDown) -> Just $ Left $ GameMenu mM $ incrementMenuCursor m
        (_, _, True, _) -> Right <$> (getNextMenu $ options m)
        _ -> Nothing
    where
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
        (_, _, True, _) -> Right <$> (getNextMenu $ options topM)
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
pauseMenu gps gd = Overlay 20 20 200 200 DarkBlue menu
    where
        menu = mkMenu words [] menuOpt
        menuOpt = MenuOptions (SelOneListOpts $ OALOpts opts (CursorRect White)) (BlockDrawInfo 50 120 5 15) 0
        words = [ TextDisplay "Game Menu" 30 30 10 White
                ]
        opts = [ MenuAction "Continue" True gps
               , MenuAction "Main Menu" True $ MainMenu gd
               , MenuAction "Save & Exit" True (GameExitState (Just gd))
               ]
