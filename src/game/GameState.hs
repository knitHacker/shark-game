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
import Graphics.TextUtil

import qualified Data.Text as T

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

import Debug.Trace
import Data.Maybe (catMaybes)

initGameState :: GameState
initGameState = GameState SplashScreen (GameView vw Nothing [TimeoutData 0 10 $ TimeoutNext $ MainMenu Nothing] Nothing) False
    where
        vw = View [] [] [] [] Nothing


--getAction :: (GameStateRead m) => m Action
--getAction = do
--    gs <- readGameState
--    case gameLastState gs of


updateGameState :: (MonadIO m, GraphicsRead m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    gr' <- readGraphics
    let gsM = updateGameView gr' inputs $ gameView gs
    case (windowResized inputs, gsM) of
        (Just resize, _) ->
            let gs' = reDrawState (gameState gs) cfgs inputs gr'
            in if exiting gs' then return gs' else return $ gs { gameView = (mergeGameViews (gameView gs) (gameView gs')) }
        (Nothing, Nothing) -> return gs
        (_, Just (Left gv)) -> return $ GameState (gameState gs) gv False
        (_, Just (Right gps)) -> do
            gs' <- liftIO $ moveToNextState gps cfgs inputs gr'
            return $ gs'


updateGameView :: Graphics -> InputState -> GameView -> Maybe (Either GameView GamePlayState)
updateGameView gr inputs gv@(GameView vl oM tL mM) = if null outs then Nothing else Just (head outs)
    where
        viewUp = viewScroll vl >>= updateGameViewScroll inputs
        overlayUp = oM >>= updateGameOverlay inputs
        timeoutUp = updateGameTimeout gr vl inputs tL
        menuUp = mM >>= updateGameMenu inputs
        out = case viewUp of
            Nothing -> Nothing
            jvs -> Just $ Left $ gv { viewLayer = (viewLayer gv) { viewScroll = jvs } }
        out2 = case overlayUp of
                Just (Left ov) -> Just $ Left $ gv { viewOverlay = Just ov }
                Just (Right gps) -> Just $ Right gps
                _ -> Nothing
        -- If overlay is open don't timeout
        out3 = case (oM, timeoutUp) of
                (Just (OverlayView True _ _), _) -> Nothing  -- Overlay is active, don't process timeouts
                (_, Just (Right gps)) -> Just $ Right gps
                (_, Just (Left (td, v'))) -> Just $ Left $ gv { viewTimeouts = td, viewLayer = v' }
                _ -> Nothing
        out4 = case menuUp of
                Just (Left m) -> Just $ Left $ gv { viewMenu = Just m }
                Just (Right gps) -> Just $ Right gps
                _ -> Nothing
        outs = catMaybes [out, out2, out4, out3]

updateGameViewScroll :: InputState -> ViewScroll GamePlayState -> Maybe (ViewScroll GamePlayState)
updateGameViewScroll i vs = case mouseInputs i of
    Just mi -> Just $ scrollView vs (scrollAmt mi)
    Nothing -> Nothing

updateGameTimeout :: Graphics -> View GamePlayState -> InputState -> [TimeoutData GamePlayState] -> Maybe (Either ([TimeoutData GamePlayState], View GamePlayState) GamePlayState)
updateGameTimeout _ _ _ [] = Nothing
updateGameTimeout gr vO i@(InputState _ _ _ ts) timeouts =
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
    | inputDirection inputs == Just DDown = Just $ incrementMenuCursor m
    | inputDirection inputs == Just DUp = Just $ decrementMenuCursor m
    | otherwise = Nothing
    where
        selected = enterJustPressed inputs
        backSelected = backJustPressed inputs

gameMenu :: GameMenu -> GameView
gameMenu (GameMenu v m) = GameView v Nothing [] (Just m)

gameMenuPause :: Graphics -> GamePlayState -> GameData -> GameMenu -> GameState
gameMenuPause gr gps gd (GameMenu v m) = GameState gps (withPause gr gps gd $ GameView v Nothing [] (Just m)) False

emptyGameView :: GameView
emptyGameView = GameView emptyView Nothing [] Nothing

reDrawState :: GamePlayState -> GameConfigs -> InputState -> Graphics -> GameState
reDrawState gps cfgs inputs gr =
    case gps of
        SplashScreen -> GameState gps (splash inputs) False
        GameExitState (Just gd) -> GameState gps emptyGameView True
        GameExitState Nothing -> GameState gps emptyGameView True
        MainMenu gdM -> GameState gps (mainMenu gdM gr) False
        IntroWelcome (Just gd)-> menuWithPause gd $ introWelcome gd gr
        IntroMission gd -> menuWithPause gd $ introMission gd gr
        IntroBoat gd -> menuWithPause gd $ introBoat gd cfgs gr
        IntroEquipment gd -> menuWithPause gd $ introEquipment gd cfgs gr
        IntroResearch gd -> menuWithPause gd $ introResearch gd gr
        IntroFunds gd -> menuWithPause gd $ introFunds gd gr
        IntroEnd gd -> menuWithPause gd $ introEnd gd gr
        ResearchCenter gd -> GameState gps (withPause gr gps gd $ researchCenterMenu gd gr) False
        TripDestinationSelect gd idx -> menuWithPause gd $ mapMenu gd idx gr cfgs
        TripEquipmentSelect gd loc eqs cp -> menuWithPause gd $ equipmentPickMenu gd loc eqs cp cfgs
        TripReview gd loc eqs -> menuWithPause gd $ reviewTripMenu gd loc eqs cfgs
        TripProgress gd tp -> GameState gps (withPause gr gps gd $ tripProgressMenu gd tp cfgs inputs gr) False
        SharkFound gd sf tp -> gameMenuPause gr gps gd $ sharkFoundMenu gd sf tp cfgs gr
        TripResults gd tp -> gameMenuPause gr gps gd $ tripResultsMenu gd tp cfgs gr
        DataReviewTop gd -> menuWithPause gd $ topReviewMenu gd cfgs
        SharkReviewTop gd mP -> menuWithPause gd $ topReviewSharksMenu gd mP cfgs gr
        SharkReview gd se -> menuWithPause gd $ sharkReviewMenu gd se cfgs gr
        ResearchReviewTop gd -> menuWithPause gd $ topLabMenu gd cfgs
        OpenResearchMenu gd -> menuWithPause gd $ openResearchMenu gd cfgs gr
        CompletedResearchMenu gd -> menuWithPause gd $ completedResearchMenu gd cfgs gr
        InvestigateResearchMenu gd rd -> menuWithPause gd $ investigateResearchMenu gd rd cfgs gr
        AwardGrantMenu gd rd -> menuWithPause gd $ awardGrantMenu gd rd cfgs gr
        CompletedResearchReviewMenu gd rd -> menuWithPause gd $ completedResearchReviewMenu gd rd cfgs gr
        LabManagement gd -> menuWithPause gd $ labTopMenu gd gr
        FundraiserTop gd -> menuWithPause gd $ fundraiserTopMenu gd cfgs gr
        FleetManagement gd -> GameState gps (withPause gr gps gd $ fleetManagementTopMenu gd cfgs inputs gr) False
        EquipmentManagement gd -> menuWithPause gd $ equipmentManagementTopMenu gd cfgs gr
        EquipmentStore popup gd -> menuWithPause gd $ equipmentStoreMenu popup gd cfgs gr
        BoatStore popup gd -> menuWithPause gd $ boatStoreMenu popup gd cfgs gr
        ChooseBoat gd -> menuWithPause gd $ chooseActiveBoatMenu gd cfgs
        ViewDonors gd -> menuWithPause gd $ donorList gd gr
        NewFundraiser gd -> menuWithPause gd $ fundraisingMenu gd cfgs gr
    where
        menuWithPause = gameMenuPause gr gps



moveToNextState :: GamePlayState -> GameConfigs -> InputState -> Graphics -> IO GameState
moveToNextState gps cfgs inputs gr =
    case gps of
        GameExitState (Just gd) -> do
            saveGame gd cfgs
            return $ (GameState gps emptyGameView True)
        GameExitState Nothing -> return (GameState gps emptyGameView True)
        MainMenu gdM -> do
            case gdM of
                Just gd -> saveGame gd cfgs
                Nothing -> return ()
            return (GameState gps (mainMenu gdM gr) False)
        IntroWelcome (Just gd) -> return $ menuWithPause gd $ introWelcome gd gr
        IntroWelcome Nothing -> do
            nGame <- startNewGame cfgs
            let gps' = IntroWelcome (Just nGame)
            return $ gameMenuPause gr gps' nGame $ introWelcome nGame gr
        IntroMission gd -> return $ menuWithPause gd $ introMission gd gr
        IntroBoat gd -> return $ menuWithPause gd $ introBoat gd cfgs gr
        IntroEquipment gd -> return $ menuWithPause gd $ introEquipment gd cfgs gr
        IntroResearch gd -> return $ menuWithPause gd $ introResearch gd gr
        IntroFunds gd -> return $ menuWithPause gd $ introFunds gd gr
        IntroEnd gd -> return $ menuWithPause gd $ introEnd gd gr
        ResearchCenter gd -> return $ GameState gps (withPause gr gps gd $ researchCenterMenu gd gr) False
        TripDestinationSelect gd idx -> return $ menuWithPause gd $ mapMenu gd idx gr cfgs
        TripEquipmentSelect gd loc eqs cp -> return $ menuWithPause gd $ equipmentPickMenu gd loc eqs cp cfgs
        TripReview gd loc eqs -> return $ menuWithPause gd $ reviewTripMenu gd loc eqs cfgs
        TripProgress gd tp -> return $ GameState gps (withPause gr gps gd $ tripProgressMenu gd tp cfgs inputs gr) False
        SharkFound gd sf tp -> return $ gameMenuPause gr gps gd $ sharkFoundMenu gd sf tp cfgs gr
        TripResults gd tp -> return $ gameMenuPause gr gps gd $ tripResultsMenu gd tp cfgs gr
        DataReviewTop gd -> return $ menuWithPause gd $ topReviewMenu gd cfgs
        SharkReviewTop gd mP -> return $ menuWithPause gd $ topReviewSharksMenu gd mP cfgs gr
        SharkReview gd se -> return $ menuWithPause gd $ sharkReviewMenu gd se cfgs gr
        ResearchReviewTop gd -> return $ menuWithPause gd $ topLabMenu gd cfgs
        OpenResearchMenu gd -> return $ menuWithPause gd $ openResearchMenu gd cfgs gr
        CompletedResearchMenu gd -> return $ menuWithPause gd $ completedResearchMenu gd cfgs gr
        InvestigateResearchMenu gd rd -> return $ menuWithPause gd $ investigateResearchMenu gd rd cfgs gr
        AwardGrantMenu gd rd -> return $ menuWithPause gd $ awardGrantMenu gd rd cfgs gr
        CompletedResearchReviewMenu gd rd -> return $ menuWithPause gd $ completedResearchReviewMenu gd rd cfgs gr
        LabManagement gd -> return $ menuWithPause gd $ labTopMenu gd gr
        FundraiserTop gd -> return $ menuWithPause gd $ fundraiserTopMenu gd cfgs gr
        FleetManagement gd -> return $ GameState gps (withPause gr gps gd $ fleetManagementTopMenu gd cfgs inputs gr) False
        EquipmentManagement gd -> return $ menuWithPause gd $ equipmentManagementTopMenu gd cfgs gr
        EquipmentStore popup gd -> return $ menuWithPause gd $ equipmentStoreMenu popup gd cfgs gr
        BoatStore popup gd -> return $ menuWithPause gd $ boatStoreMenu popup gd cfgs gr
        ChooseBoat gd -> return $ menuWithPause gd $ chooseActiveBoatMenu gd cfgs
        ViewDonors gd -> return $ menuWithPause gd $ donorList gd gr
        NewFundraiser gd -> return $ menuWithPause gd $ fundraisingMenu gd cfgs gr
    where
        menuWithPause = gameMenuPause gr gps


saveGame :: GameData -> GameConfigs -> IO ()
saveGame gd cfgs = do
    saveToFile gd
    updateStateConfigs sc
    where
        sc = (stateCfgs cfgs) { lastSaveM = Just (gameDataSaveFile gd) }

mainMenuView :: GameConfigs -> OutputHandles -> Graphics -> InputState -> IO GameState
mainMenuView cfgs outs gr inputs = do
    gdM <- case lastSaveM (stateCfgs cfgs) of
                Nothing -> return Nothing
                Just sf -> do
                    gdE <- loadFromFile sf
                    case gdE of
                        Left err -> do
                            putStrLn $ T.unpack err
                            return Nothing
                        Right gd -> return $ Just gd
    return $ GameState (MainMenu gdM) (mainMenu gdM gr) False


withPause :: Graphics -> GamePlayState -> GameData -> GameView -> GameView
withPause gr gps gd gv = gv { viewOverlay = Just $ pauseMenu gr gps gd }

pauseMenu :: Graphics -> GamePlayState -> GameData -> OverlayView GamePlayState
pauseMenu gr gps gd = OverlayView False (textView words) (Overlay overlayX overlayY 750 600 DarkBlue menuOpt)
    where
        overlayX = midStartX gr 750
        overlayY = midStartY gr 600
        menuOpt = MenuData (SelOneListOpts $ OALOpts opts Nothing Nothing (CursorRect White)) (BlockDrawInfo (overlayX + 150) (overlayY + 300) 4 15) 0
        words = [ TextDisplay "Game Menu" (fromIntegral (overlayX + 50)) (fromIntegral (overlayY + 50)) 8 White Nothing
                ]
        opts = [ MenuAction "Continue" Nothing $ Just gps
               , MenuAction "Main Menu" Nothing $ Just $ MainMenu $ Just gd
               , MenuAction "Save & Exit" Nothing $ Just (GameExitState (Just gd))
               ]

introWelcomeIO :: GameConfigs -> Graphics -> IO GameView
introWelcomeIO cfgs gr = do
    nGame <- startNewGame cfgs
    return $ gameMenu $ introWelcome nGame gr
