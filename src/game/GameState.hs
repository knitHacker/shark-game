{-# LANGUAGE OverloadedStrings #-}

module GameState
    ( getGameUpdate
    , drawOverlay
    , moveTo
    ) where

import InputState
import GameState.Types
import GameState.Helpers
import GameState.Menu.GameMenus ( initialMainMenuState, initialResearchCenterState )
import GameState.Menu.TripMenus ( initialTripDestinationSelect )
import Graphics.Types
import Graphics.TextUtil
import Graphics.Menu
import OutputHandles.Types ( Color(..), TextDisplay(TextDisplay) )
import Configs ( GameConfigs )
import SaveData ( GameData )


getGameUpdate :: GameConfigs -> Graphics -> InputResult -> GameState -> Update
getGameUpdate cfgs gr inputRes gs =
    case gameOverlay gs of
        Just overlay -> handleOverlayInput (newInputs inputRes) overlay
        Nothing      -> anyThink (gameCurrentState gs) cfgs inputRes gr


handleOverlayInput :: InputState -> GameOverlay -> Update
handleOverlayInput inputs (PauseOverlay cursor gd)
    | escapeJustPressed inputs               = PureStep $ SetOverlay Nothing
    | inputRepeating inputs               = PureStep UseCache
    | enterJustPressed inputs             = selectPauseOption cursor gd
    | inputDirection inputs == Just DDown = PureStep $ SetOverlay $ Just $ PauseOverlay (min (cursor + 1) 2) gd
    | inputDirection inputs == Just DUp   = PureStep $ SetOverlay $ Just $ PauseOverlay (max (cursor - 1) 0) gd
    | otherwise                           = PureStep UseCache


selectPauseOption :: Int -> GameData -> Update
selectPauseOption 0 _  = PureStep $ SetOverlay Nothing
selectPauseOption 1 gd = SaveFile gd $ Transition $ initialMainMenuState (Just gd)
selectPauseOption 2 gd = SaveAndExit gd
selectPauseOption _ _  = PureStep UseCache


drawOverlay :: GameOverlay -> Graphics -> OverlayView Update
drawOverlay (PauseOverlay cursor gd) gr = OverlayView True (textView words) overlayMenu
    where
        overlayX  = midStartX gr 750
        overlayY  = midStartY gr 600
        words     = [ TextDisplay "Game Menu" (fromIntegral (overlayX + 50)) (fromIntegral (overlayY + 50)) 8 White Nothing ]
        opts      = [ MenuAction "Continue"    Nothing $ Just $ PureStep $ SetOverlay Nothing
                    , MenuAction "Main Menu"   Nothing $ Just $ SaveFile gd $ Transition $ initialMainMenuState (Just gd)
                    , MenuAction "Save & Exit" Nothing $ Just $ SaveAndExit gd
                    ]
        menuData  = selOneOpts (overlayX + 150) (overlayY + 300) 4 15 opts Nothing (CursorRect White) cursor
        overlayMenu = Overlay overlayX overlayY 750 600 DarkBlue menuData

moveTo :: GameSection -> AnyGamePlayState
moveTo (ResearchCenter gd) = initialResearchCenterState gd
moveTo (Trip gd) = initialTripDestinationSelect gd
moveTo (DataReview gd) = undefined -- TODO: this goes to menus in DataReviewMenu
moveTo (LabManagement gd) = undefined -- TODO: this goes to menus in LabMenus