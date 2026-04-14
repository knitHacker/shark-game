{-# LANGUAGE OverloadedStrings #-}

-- Generic input and animation helpers for GamePlayState instances.
-- Lives here (not in GameState.hs) to avoid the circular import:
--   GameState.hs → GameState.Menu.* → GameState.Helpers

module GameState.Helpers
    ( updateMenu
    , updateOverlay
    , updateOverlayMenu
    , updateScroll
    , stepTimeouts
    , gameMenuView
    , gameMenuWithOverlay
    , thinkWithPause
    ) where

import InputState
import Graphics.Types hiding (updateMenu)
import Graphics.Menu
import Graphics.Animation
import GameState.Types
import SaveData ( GameData )


-- | Update a menu based on input.
-- Returns Nothing if nothing changed; Just (Left menu') for cursor movement;
-- Just (Right next) when an option is selected.
updateMenu :: InputState -> Menu a -> Maybe (Either (Menu a) a)
updateMenu inputs m
    | selected     = Right <$> getNextMenu m
    | backSelected = Right <$> getBackOption m
    | inputRepeating inputs = Nothing
    | inputDirection inputs == Just DDown = Just $ incrementMenuCursor m
    | inputDirection inputs == Just DUp   = Just $ decrementMenuCursor m
    | otherwise    = Nothing
    where
        selected     = enterJustPressed inputs
        backSelected = backJustPressed inputs


-- | Update a pause/overlay based on input.
updateOverlay :: InputState -> OverlayView a -> Maybe (Either (OverlayView a) a)
updateOverlay i ov@(OverlayView active _ om)
    | inputRepeating i = Nothing
    | escapePressed i  = Just $ Left $ ov { isActive = not active }
    | not active       = Nothing
    | otherwise = case updateOverlayMenu i om of
        Just (Left om') -> Just $ Left $ ov { overlayMenu = om' }
        Just (Right next) -> Just $ Right next
        _ -> Nothing


-- | Update the cursor inside an overlay menu.
updateOverlayMenu :: InputState -> OverlayMenu a -> Maybe (Either (OverlayMenu a) a)
updateOverlayMenu inputs om@(Overlay _ _ _ _ _ md)
    | enterJustPressed inputs             = Right <$> getNextOption md
    | inputRepeating inputs               = Nothing
    | inputDirection inputs == Just DDown = Just $ Left $ om { overlayData = incrementMenuOpt md }
    | inputDirection inputs == Just DUp   = Just $ Left $ om { overlayData = decrementMenuOpt md }
    | otherwise                           = Nothing


-- | Update a scroll view based on mouse input.
updateScroll :: InputState -> ViewScroll a -> Maybe (ViewScroll a)
updateScroll i vs = case mouseInputs i of
    Just mi -> Just $ scrollView vs (scrollAmt mi)
    Nothing -> Nothing


-- | Advance a list of timeouts by one frame.
-- Returns the updated timeout list and, if a TimeoutNext fired, the next state.
stepTimeouts :: Graphics -> InputState -> [TimeoutData a] -> ([TimeoutData a], Maybe a)
stepTimeouts _ inputs timeouts = foldr check ([], Nothing) timeouts
    where
        ts = timestamp inputs
        check td (acc, Just next) = (td : acc, Just next)
        check td@(TimeoutData lTO toLen ta) (acc, Nothing)
            | ts - lTO > toLen = case ta of
                TimeoutNext next -> (acc, Just next)
                TimeoutAnimation _ ->
                    (td { lastTimeout = ts } : acc, Nothing)
            | otherwise = (td : acc, Nothing)


-- | Build a GameView from a GameMenu (view layer + menu, no overlay).
gameMenuView :: GameMenu -> GameView
gameMenuView (GameMenu v m) = GameView v Nothing (Just m)


-- | Build a GameView from a GameMenu with a pause overlay attached.
gameMenuWithOverlay :: OverlayView Update -> GameMenu -> GameView
gameMenuWithOverlay ov (GameMenu v m) = GameView v (Just ov) (Just m)


-- | Wrap a think result to intercept ESC and open the pause overlay.
-- Only call this from states where canPause = True.
thinkWithPause :: GameData -> InputResult -> Update -> Update
thinkWithPause gd inputRes innerUpdate
    | escapeJustPressed (newInputs inputRes) = PureStep $ SetOverlay $ Just $ PauseOverlay 0 gd
    | otherwise                           = innerUpdate
