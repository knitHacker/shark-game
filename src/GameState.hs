{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module GameState
    ( initGameState
    , isGameExiting
    , updateGameState
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
import Graphics.Menu

import qualified Data.Text as T

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

import Debug.Trace

initGameState :: TextureFileMap -> GameConfigs -> OutputHandles -> IO GameState
initGameState tm cfgs outs = do
    graph <- initGraphics tm outs
    gv <- mainMenuView cfgs outs
    return $ GameState graph gv Nothing

isGameExiting :: GameState -> Bool
isGameExiting (GameState _ GameExiting _) = True
isGameExiting _ = False

updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    let gsM = case gameView gs of
                GameMenu mm m -> updateGameStateInMenu mm m inputs
                OverlayMenu tm bm -> updateGameStateInOverlay tm bm inputs
                GameTimeout mm tv -> updateGameTimeout mm tv inputs
                _ -> Nothing
    case gsM of
        Nothing -> return gs
        Just (Left gv) -> return $ GameState (gameGraphics gs) gv Nothing
        Just (Right gps) -> do
            gv <- liftIO $ moveToNextState gps cfgs inputs $ gameGraphics gs
            return $ GameState (gameGraphics gs) gv Nothing


updateGameTimeout :: Maybe (OverlayMenu GamePlayState) -> TimeoutView GamePlayState -> InputState -> Maybe (Either GameView GamePlayState)
updateGameTimeout mM tv i@(InputState _ _ ts) =
    case (esc, mM, to) of
        (True, Just om, _) -> Just $ Left $ OverlayMenu om $ BasicTimeoutView tv
        (_, _, True) -> Just $ Right $ timeoutAction tv
        _ -> Nothing
    where
        esc = escapeJustPressed i
        to = ts - lastTimeout tv > timeoutLength tv

moveToNextState :: GamePlayState -> GameConfigs -> InputState -> Graphics -> IO GameView
moveToNextState gps cfgs inputs gr =
    case gps of
        GameExitState (Just gd) -> do
            saveGame gd cfgs
            return GameExiting
        GameExitState Nothing -> return GameExiting
        MainMenu gd -> do
            saveGame gd cfgs
            return $ GameMenu Nothing $ mainMenu $ Just gd
        IntroPage -> introPageIO cfgs
        ResearchCenter gd -> return $ menuWithPause gd $ researchCenterMenu gd gr
        TripDestinationSelect gd -> return $ menuWithPause gd $ mapMenu gd cfgs
        TripEquipmentSelect gd loc eqs cp -> return $ menuWithPause gd $ equipmentPickMenu gd loc eqs cp cfgs
        TripReview gd loc eqs -> return $ menuWithPause gd $ reviewTripMenu gd loc eqs cfgs
        TripProgress gd tp -> return $ withPause gps gd $ BasicTimeoutView $ tripProgressMenu gd tp cfgs inputs
        SharkFound gd sf tp -> return $ GameMenu Nothing $ sharkFoundMenu gd sf tp cfgs
        TripResults gd tp -> return $ GameMenu Nothing $ tripResultsMenu gd tp cfgs gr
        DataReviewTop gd -> return $ menuWithPause gd $ topReviewMenu gd cfgs
        SharkReviewTop gd mP -> return $ menuWithPause gd $ topReviewSharksMenu gd mP cfgs
        SharkReview gd se -> return $ menuWithPause gd $ sharkReviewMenu gd se cfgs gr
        ResearchReviewTop gd -> return $ menuWithPause gd $ topLabMenu gd cfgs
        OpenResearchMenu gd -> return $ menuWithPause gd $ openResearchMenu gd cfgs
        CompletedResearchMenu gd -> return $ menuWithPause gd $ completedResearchMenu gd cfgs
        InvestigateResearchMenu gd rd -> return $ menuWithPause gd $ investigateResearchMenu gd rd cfgs
        AwardGrantMenu gd rd -> return $ menuWithPause gd $ awardGrantMenu gd rd cfgs gr
        CompletedResearchReviewMenu gd rd -> return $ menuWithPause gd $ completedResearchReviewMenu gd rd cfgs gr
        LabManagement gd -> return $ menuWithPause gd $ labTopMenu gd gr
        FleetManagement gd -> return $ menuWithPause gd $ fleetManagementTopMenu gd cfgs gr
        EquipmentManagement gd -> return $ menuWithPause gd $ equipmentManagementTopMenu gd cfgs gr
        EquipmentStore gd popup -> return $ menuWithPause gd $ equipmentStoreMenu gd popup cfgs gr
    where
        menuWithPause gd bm = withPause gps gd $ BasicMenu bm


saveGame :: GameData -> GameConfigs -> IO ()
saveGame gd cfgs = do
    saveToFile gd
    updateStateConfigs sc
    where
        sc = (stateCfgs cfgs) { lastSaveM = Just (gameDataSaveFile gd) }

mainMenuView :: GameConfigs -> OutputHandles -> IO GameView
mainMenuView cfgs outs = do
    gdM <- case lastSaveM (stateCfgs cfgs) of
                Nothing -> return Nothing
                Just sf -> do
                    gdE <- loadFromFile sf
                    case gdE of
                        Left err -> do
                            putStrLn $ T.unpack err
                            return Nothing
                        Right gd -> return $ Just gd
    return $ GameMenu Nothing $ mainMenu gdM


withPause :: GamePlayState -> GameData -> BasicView GamePlayState -> GameView
withPause gps gd (BasicMenu m) = GameMenu (Just (pauseMenu gps gd)) m
withPause gps gd (BasicTimeoutView m) = GameTimeout (Just (pauseMenu gps gd)) m


updateGameStateInMenu :: Maybe (OverlayMenu GamePlayState) -> Menu GamePlayState -> InputState -> Maybe (Either GameView GamePlayState)
updateGameStateInMenu _ _ (InputState Nothing Nothing _) = Nothing
updateGameStateInMenu mM m inputs =
    case (esc, mM, selected, moveDirM, mouseInputs inputs) of
        (True, Just om, _, _, _) -> Just $ Left $ OverlayMenu om $ BasicMenu m
        (_, _, _, Just DUp, _) -> Just $ Left $ GameMenu mM $ decrementMenuCursor m
        (_, _, _, Just DDown, _) -> Just $ Left $ GameMenu mM $ incrementMenuCursor m
        (_, _, True, _, _) -> Right <$> getNextMenu m
        (_, _, _, _, Just (MouseInputs scroll)) ->
            if isMenuScrollable m
                then Just $ Left $ GameMenu mM $ scrollMenu m scroll
                else Nothing
        _ -> Nothing
    where
        moveDirM = if inputRepeating inputs then Nothing else inputDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        optLen = optionLength $ options m

updateGameStateInOverlay :: OverlayMenu GamePlayState -> BasicView GamePlayState -> InputState -> Maybe (Either GameView GamePlayState)
updateGameStateInOverlay _ _ (InputState Nothing Nothing _) = Nothing
updateGameStateInOverlay om@(Overlay _ _ _ _ _ topM) backM inputs =
    case (esc, backM, selected, moveDirM) of
        (True, BasicMenu m, _, _) -> Just $ Left $ GameMenu (Just (om' topM)) m
        (True, BasicTimeoutView tov, _, _) -> Just $ Left $ GameTimeout (Just (om' topM)) tov
        (_, _, True, _) -> Right <$> getNextMenu topM
        (_, _, _, Just DUp) -> Just $ Left $ OverlayMenu (om' (decrementMenuCursor topM)) backM
        (_, _, _, Just DDown) -> Just $ Left $ OverlayMenu (om' (incrementMenuCursor topM)) backM
        _ -> Nothing
    where
        moveDirM = if inputRepeating inputs then Nothing else inputDirection inputs
        selected = enterJustPressed inputs
        esc = escapeJustPressed inputs
        om' newM = om { overlayMenu = newM }

pauseMenu :: GamePlayState -> GameData -> OverlayMenu GamePlayState
pauseMenu gps gd = Overlay 20 20 200 200 DarkBlue menu
    where
        menu = mkMenu words [] Nothing menuOpt
        menuOpt = MenuOptions (SelOneListOpts $ OALOpts opts (CursorRect White)) (BlockDrawInfo 50 120 5 15) 0
        words = [ TextDisplay "Game Menu" 30 30 10 White
                ]
        opts = [ MenuAction "Continue" $ Just gps
               , MenuAction "Main Menu" $ Just $ MainMenu gd
               , MenuAction "Save & Exit" $ Just (GameExitState (Just gd))
               ]

introPageIO :: GameConfigs -> IO GameView
introPageIO cfgs = do
    nGame <- startNewGame cfgs
    return $ GameMenu Nothing $ introPage nGame cfgs
