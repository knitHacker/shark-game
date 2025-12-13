{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module GameState
    ( initGameState
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
import Graphics.Animation

import qualified Data.Text as T

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

import Debug.Trace
import Data.Maybe (catMaybes)

initGameState :: TextureCfg -> GameConfigs -> OutputHandles -> IO GameState
initGameState tm cfgs outs = do
    graph <- initGraphics tm outs
    gv <- mainMenuView cfgs outs
    return $ GameState graph gv Nothing


updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    let gsM = updateGameView (gameGraphics gs) inputs $ gameView gs
    case gsM of
        Nothing -> return gs
        Just (Left gv) -> return $ GameState (gameGraphics gs) gv Nothing
        Just (Right gps) -> do
            gv <- liftIO $ moveToNextState gps cfgs inputs $ gameGraphics gs
            return $ GameState (gameGraphics gs) gv Nothing


updateGameView :: Graphics -> InputState -> GameDrawInfo -> Maybe (Either GameDrawInfo GamePlayState)
updateGameView _ _ GameExiting = Nothing
updateGameView gr inputs (GameViewInfo gv@(GameView vl oM tL mM)) = if null outs then Nothing else Just (head outs)
    where
        viewUp = viewScroll vl >>= updateGameViewScroll inputs
        overlayUp = oM >>= updateGameOverlay inputs
        timeoutUp = updateGameTimeout gr vl inputs tL
        menuUp = mM >>= updateGameMenu inputs
        out = case viewUp of
            Nothing -> Nothing
            jvs -> Just $ Left $ GameViewInfo $ gv { viewLayer = (viewLayer gv) { viewScroll = jvs } }
        out2 = case overlayUp of
                Just (Left ov) -> Just $ Left $ GameViewInfo $ gv { viewOverlay = Just ov }
                Just (Right gps) -> Just $ Right gps
                _ -> Nothing
        -- If overlay is open don't timeout
        out3 = case (oM, timeoutUp) of
                (Just (OverlayView False _ _), Just (Right gps)) -> Just $ Right gps
                (Just (OverlayView False _ _), Just (Left (td, v'))) -> Just $ Left $ GameViewInfo $ gv { viewTimeouts = td, viewLayer = v' }
                _ -> Nothing
        out4 = case menuUp of
                Just (Left m) -> Just $ Left $ GameViewInfo $ gv { viewMenu = Just m }
                Just (Right gps) -> Just $ Right gps
                _ -> Nothing
        outs = catMaybes [out, out2, out4, out3]

updateGameViewScroll :: InputState -> ViewScroll GamePlayState -> Maybe (ViewScroll GamePlayState)
updateGameViewScroll i vs = case mouseInputs i of
    Just mi -> Just $ scrollView vs (scrollAmt mi)
    Nothing -> Nothing

updateGameTimeout :: Graphics -> View GamePlayState -> InputState -> [TimeoutData GamePlayState] -> Maybe (Either ([TimeoutData GamePlayState], View GamePlayState) GamePlayState)
updateGameTimeout _ _ _ [] = Nothing
updateGameTimeout gr vO i@(InputState _ _ ts) timeouts =
    case outs of
        Left (tds, v') -> Just $ Left (tds, v')
        Right gps -> Just $ Right gps
    where
        outs = foldr checkTimeout (Left ([], vO)) timeouts
        checkTimeout _ (Right gps) = Right gps
        checkTimeout td@(TimeoutData lTO toLen ta) (Left (tds, v'))
            | ts - lTO > toLen = case ta of
                TimeoutNext gps -> Right gps
                TimeoutAnimation animData ->
                    let (ad, v'') = updateAnimation gr v' animData
                    in Left (tds++[td { lastTimeout = ts, timeoutAction = TimeoutAnimation ad }], v'')
            | otherwise = Left (tds++[td], v')

updateGameOverlay :: InputState -> OverlayView GamePlayState -> Maybe (Either (OverlayView GamePlayState) GamePlayState)
updateGameOverlay i ov@(OverlayView active _ om)
    | inputRepeating i = Nothing
    | escapePressed i = Just $ Left $ ov { isActive = not active }
    | not active = Nothing
    | otherwise = case updateOverlayMenu i om of
                    Just (Left om') -> Just $ Left $ ov { overlayMenu = om' }
                    Just (Right gps) -> Just $ Right gps
                    _ -> Nothing

updateOverlayMenu :: InputState -> OverlayMenu GamePlayState -> Maybe (Either (OverlayMenu GamePlayState) GamePlayState)
updateOverlayMenu inputs om@(Overlay _ _ _ _ _ md)
    | enterJustPressed inputs = Right <$> getNextOption md
    | inputRepeating inputs = Nothing
    | inputDirection inputs == Just DDown = Just $ Left $ om { overlayData = incrementMenuOpt md }
    | inputDirection inputs == Just DUp = Just $ Left $ om { overlayData = decrementMenuOpt md }
    | otherwise = Nothing


updateGameMenu :: InputState -> Menu GamePlayState -> Maybe (Either (Menu GamePlayState) GamePlayState)
updateGameMenu inputs m
    | selected = Right <$> getNextMenu m
    | backSelected = Right <$> getBackOption m
    | inputRepeating inputs = Nothing
    | inputDirection inputs == Just DDown = Just $ Left $ incrementMenuCursor m
    | inputDirection inputs == Just DUp = Just $ Left $ decrementMenuCursor m
    | otherwise = Nothing
    where
        selected = enterJustPressed inputs
        backSelected = backJustPressed inputs

gameMenu :: GameMenu -> GameDrawInfo
gameMenu (GameMenu v m) = GameViewInfo $ GameView v Nothing [] (Just m)

gameMenuPause :: GamePlayState -> GameData -> GameMenu -> GameDrawInfo
gameMenuPause gps gd (GameMenu v m) = withPause gps gd $ GameView v Nothing [] (Just m)

moveToNextState :: GamePlayState -> GameConfigs -> InputState -> Graphics -> IO GameDrawInfo
moveToNextState gps cfgs inputs gr =
    case gps of
        GameExitState (Just gd) -> do
            saveGame gd cfgs
            return GameExiting
        GameExitState Nothing -> return GameExiting
        MainMenu gd -> do
            saveGame gd cfgs
            return $ gameMenu $ mainMenu $ Just gd
        IntroPage -> introPageIO cfgs
        ResearchCenter gd -> return $ menuWithPause gd $ researchCenterMenu gd gr
        TripDestinationSelect gd -> return $ menuWithPause gd $ mapMenu gd cfgs
        TripEquipmentSelect gd loc eqs cp -> return $ menuWithPause gd $ equipmentPickMenu gd loc eqs cp cfgs
        TripReview gd loc eqs -> return $ menuWithPause gd $ reviewTripMenu gd loc eqs cfgs
        TripProgress gd tp -> return $ withPause gps gd $ tripProgressMenu gd tp cfgs inputs gr
        SharkFound gd sf tp -> return $ gameMenuPause gps gd $ sharkFoundMenu gd sf tp cfgs
        TripResults gd tp -> return $ gameMenuPause gps gd $ tripResultsMenu gd tp cfgs gr
        DataReviewTop gd -> return $ menuWithPause gd $ topReviewMenu gd cfgs
        SharkReviewTop gd mP -> return $ menuWithPause gd $ topReviewSharksMenu gd mP cfgs
        SharkReview gd se -> return $ menuWithPause gd $ sharkReviewMenu gd se cfgs gr
        ResearchReviewTop gd -> return $ menuWithPause gd $ topLabMenu gd cfgs
        OpenResearchMenu gd -> return $ menuWithPause gd $ openResearchMenu gd cfgs
        CompletedResearchMenu gd -> return $ menuWithPause gd $ completedResearchMenu gd cfgs
        InvestigateResearchMenu gd rd -> return $ menuWithPause gd $ investigateResearchMenu gd rd cfgs gr
        AwardGrantMenu gd rd -> return $ menuWithPause gd $ awardGrantMenu gd rd cfgs gr
        CompletedResearchReviewMenu gd rd -> return $ menuWithPause gd $ completedResearchReviewMenu gd rd cfgs gr
        LabManagement gd -> return $ menuWithPause gd $ labTopMenu gd gr
        FundraiserTop gd -> return $ menuWithPause gd $ fundraiserTopMenu gd cfgs gr
        FleetManagement gd -> return $ withPause gps gd $ fleetManagementTopMenu gd cfgs inputs gr
        EquipmentManagement gd -> return $ menuWithPause gd $ equipmentManagementTopMenu gd cfgs gr
        EquipmentStore popup gd -> return $ menuWithPause gd $ equipmentStoreMenu popup gd cfgs gr
        BoatStore popup gd -> return $ menuWithPause gd $ boatStoreMenu popup gd cfgs gr
        ChooseBoat gd -> return $ menuWithPause gd $ chooseActiveBoatMenu gd cfgs
        ViewDonors gd -> return $ menuWithPause gd $ donorList gd gr
        NewFundraiser gd -> return $ menuWithPause gd $ fundraisingMenu gd cfgs gr
    where
        menuWithPause = gameMenuPause gps


saveGame :: GameData -> GameConfigs -> IO ()
saveGame gd cfgs = do
    saveToFile gd
    updateStateConfigs sc
    where
        sc = (stateCfgs cfgs) { lastSaveM = Just (gameDataSaveFile gd) }

mainMenuView :: GameConfigs -> OutputHandles -> IO GameDrawInfo
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
    return $ gameMenu $ mainMenu gdM


withPause :: GamePlayState -> GameData -> GameView -> GameDrawInfo
withPause gps gd gv = GameViewInfo $ gv { viewOverlay = Just $ pauseMenu gps gd }

pauseMenu :: GamePlayState -> GameData -> OverlayView GamePlayState
pauseMenu gps gd = OverlayView False (textView words) (Overlay 300 100 750 600 DarkBlue menuOpt)
    where
        menuOpt = MenuData (SelOneListOpts $ OALOpts opts Nothing (CursorRect White)) (BlockDrawInfo 450 400 4 15) 0
        words = [ TextDisplay "Game Menu" 350 150 8 White Nothing
                ]
        opts = [ MenuAction "Continue" $ Just gps
               , MenuAction "Main Menu" $ Just $ MainMenu gd
               , MenuAction "Save & Exit" $ Just (GameExitState (Just gd))
               ]

introPageIO :: GameConfigs -> IO GameDrawInfo
introPageIO cfgs = do
    nGame <- startNewGame cfgs
    return $ gameMenu $ introPage nGame
