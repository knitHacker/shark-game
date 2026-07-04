{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, gets, modify)
import Options.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Word (Word32)

import OutputHandles (initOutputHandles, executeDraw, cleanupOutputHandles)
import OutputHandles.Types (OutputHandles, OutputRead(..), RenderAction(..), ToRender)
import Graphics (initGraphics)
import Graphics.Types (Graphics(..), GraphicsRead(..), GraphicsUpdate(..))
import Graphics.Draw (drawAssets)
import GameState (stepGameState, doTransition)
import GameState.Types
import GameState.Menu.GameMenus
import GameState.Menu.TripMenus
import GameState.Menu.LabMenus
import GameState.Menu.DataReviewMenu
import InputState
import SaveData (GameData(..), GameDataEquipment(..), loadFromFile)
import Generate (defaultGameData)
import Game (runGame)
import Shark.Trip (initTripProgress)
import Shark.Types
import Shark.Review (getKnownResearch)
import Util
import Configs

data PreviewEnvData = PreviewEnvData
    { previewConfigs       :: !GameConfigs
    , previewOutputHandles :: !OutputHandles
    , previewInputState    :: !InputState
    , previewGraphics      :: !Graphics
    , previewGameState     :: !GameState
    }

newtype PreviewEnv a = PreviewEnv (StateT PreviewEnvData IO a)
    deriving newtype
        ( Functor, Applicative, Monad
        , MonadState PreviewEnvData
        , MonadIO
        )

runPreviewEnv :: PreviewEnv a -> PreviewEnvData -> IO a
runPreviewEnv (PreviewEnv s) = evalStateT s

instance OutputRead PreviewEnv where
    getOutputs = gets previewOutputHandles

instance ConfigsRead PreviewEnv where
    readConfigs = gets previewConfigs

instance GraphicsRead PreviewEnv where
    readGraphics = gets previewGraphics

instance GameStateRead PreviewEnv where
    readGameState = gets previewGameState

instance InputRead PreviewEnv where
    readInputState = gets previewInputState

instance InputUpdate PreviewEnv where
    updateInputState = do
        fr <- readFrameRate
        old <- readInputState
        new <- updateInput (frameTime fr) old
        modify $ \pe -> pe { previewInputState = new }
        return new

instance GraphicsUpdate PreviewEnv where
    updateWindowSize Nothing = return ()
    updateWindowSize (Just (w, h)) = do
        gr <- readGraphics
        modify $ \pe -> pe { previewGraphics = gr { graphicsWindowWidth = w, graphicsWindowHeight = h } }
    updateFont fs = do
        gr <- readGraphics
        modify $ \pe -> pe { previewGraphics = gr { graphicsFontSize = fs } }

instance RenderAction PreviewEnv where
    drawRender = executeDraw
    cleanupRenderer = getOutputs >>= liftIO . cleanupOutputHandles

instance GameStateStep PreviewEnv where
    getAction :: PreviewEnv Action
    getAction = do
        gse <- readGameState
        cfgs <- readConfigs
        inputs <- readInputState
        case gameStateE gse of
            AnyGamePlayState gps -> return $ think gps cfgs inputs

    executeAction :: Action -> PreviewEnv (Maybe GameStep)
    executeAction (Exit _)    = return Nothing
    executeAction (Step step) = return $ Just step
    executeAction _           = return $ Just NoChange

    stepGame :: GameStep -> PreviewEnv ToRender
    stepGame (Transition s) = do
        liftIO $ putStrLn $ "Would transition to: " ++ show s
        gr <- readGraphics
        gs <- readGameState
        return $ drawAssets gr $ gView gs
    stepGame (TopTransition sec _) = do
        liftIO $ putStrLn $ "Would transition to: " ++ show sec
        gr <- readGraphics
        gs <- readGameState
        return $ drawAssets gr $ gView gs
    stepGame step = do
        cfgs <- readConfigs
        gr <- readGraphics
        gs <- readGameState
        let gsn' = stepGameState step gs cfgs gr
        modify $ \pe -> pe { previewGameState = gsn' }
        return $ drawAssets gr $ gView gsn'

frameTime :: Word32 -> Word32
frameTime fps = div 1000 (fps + 1)

data PreviewArgs = PreviewArgs
    { argStateName  :: String
    , argSaveFile   :: Maybe FilePath
    , argSharkCount :: Maybe Int
    }

previewArgs :: Parser PreviewArgs
previewArgs = PreviewArgs
    <$> argument str
            ( metavar "STATE"
           <> help "State to preview. One of: main-menu, intro-welcome, intro-mission, intro-boat, intro-equip, intro-research, intro-funds, intro-end, research-center, trip-map, trip-equip, trip-review, trip-progress, trip-shark-found, trip-no-shark, trip-result, lab-management-top, lab-fundraiser-top, lab-donor-list, lab-fundraising, lab-fleet-management, lab-boat-store, lab-equip-management, lab-equip-store, data-review-top, data-review-sharks, data-review-shark, data-review-research, data-review-open-research, data-review-completed-research, data-review-investigate, data-review-award-grant, data-review-completed-review"
            )
    <*> optional (argument str
            ( metavar "SAVE-FILE"
           <> help "Save file to load instead of default game data"
            ))
    <*> optional (option auto
            ( long "sharks"
           <> metavar "N"
           <> help "Number of sharks caught (trip-result only)"
            ))

main :: IO ()
main = do
    args <- execParser $ info (previewArgs <**> helper)
                              (fullDesc <> progDesc "Preview a game state")
    (tm, cfgs) <- initConfigs
    outs <- initOutputHandles tm cfgs
    gr <- initGraphics tm outs
    gd <- loadGameData cfgs $ maybe [] pure $ argSaveFile args
    case buildInitialState args cfgs gr gd of
        Nothing  -> putStrLn $ "Unknown state: " ++ argStateName args

        Just gsn -> do
            inputs <- initInputState
            runPreviewEnv runGame $ PreviewEnvData cfgs outs inputs gr gsn

loadGameData :: GameConfigs -> [String] -> IO GameData
loadGameData cfgs (fp:_) = do
    result <- loadFromFile fp
    case result of
        Right gd -> return gd
        Left err -> do
            putStrLn $ "Failed to load save: " ++ T.unpack err ++ ", using default"
            defaultGameData cfgs
loadGameData cfgs [] = defaultGameData cfgs

mkSharkFind :: Int -> GameConfigs -> TripState -> SharkFind
mkSharkFind month cfgs tripState = SharkFind month
    (getEntry (sharks $ sharkCfgs cfgs) $ head $ M.keys $ sharks $ sharkCfgs cfgs)
    (tripDestination $ trip tripState)
    (getEntry (equipment $ sharkCfgs cfgs) "fishingRod")

buildInitialState :: PreviewArgs -> GameConfigs -> Graphics -> GameData -> Maybe GameState
buildInitialState args cfgs gr gd = build (argStateName args)
    where
        build "main-menu"          = Just $ doTransition (MainMenuState (Just gd) ContinueMain []) cfgs gr
        build "intro-welcome"      = Just $ doTransition (IntroState gd IntroWelcomePage) cfgs gr
        build "intro-mission"      = Just $ doTransition (IntroState gd IntroMissionPage) cfgs gr
        build "intro-boat"         = Just $ doTransition (IntroState gd IntroBoatPage) cfgs gr
        build "intro-equip"        = Just $ doTransition (IntroState gd IntroEquipPage) cfgs gr
        build "intro-research"     = Just $ doTransition (IntroState gd IntroResearchPage) cfgs gr
        build "intro-funds"        = Just $ doTransition (IntroState gd IntroFundsPage) cfgs gr
        build "intro-end"          = Just $ doTransition (IntroState gd IntroEndPage) cfgs gr
        build "research-center"    = Just $ doTransition (initResearchCenter gd) cfgs gr
        build "trip-map"           = Just $ doTransition (initTripMapState gd) cfgs gr
        build "trip-equip"         = Just $ doTransition (initEquipPickState gd 1 "estuary") cfgs gr
        build "trip-review"        = Just $ doTransition (initTripReviewState gd 1 "estuary" (S.singleton (0, "fishingRod"))) cfgs gr
        build "trip-progress"      = Just $ doTransition (initTripProgressState gd' ta tripState 0) cfgs gr
            where
                (gd', tripState) = initTripProgress gd (gameCurrentRegion gd) "estuary" (gameActiveBoat (gameDataEquipment gd)) ["fishingRod"] cfgs
                ta = TripAttempt 1 $ getEntry (equipment (sharkCfgs cfgs)) "fishingRod"
        build "trip-shark-found"   = Just $ doTransition (SharkFoundState gd' (Just sharkFind) tripState) cfgs gr
            where
                (gd', tripState) = initTripProgress gd (gameCurrentRegion gd) "estuary" (gameActiveBoat (gameDataEquipment gd)) ["fishingRod"] cfgs
                sharkFind = mkSharkFind 1 cfgs tripState
        build "trip-no-shark"      = Just $ doTransition (SharkFoundState gd' Nothing tripState) cfgs gr
            where
                (gd', tripState) = initTripProgress gd (gameCurrentRegion gd) "estuary" (gameActiveBoat (gameDataEquipment gd)) ["fishingRod"] cfgs
        build "trip-result"        = Just $ doTransition (initTripResultState gd' tripState') cfgs gr
            where
                (gd', tripState) = initTripProgress gd (gameCurrentRegion gd) "estuary" (gameActiveBoat (gameDataEquipment gd)) ["fishingRod"] cfgs
                n = maybe 0 id $ argSharkCount args
                sharkFind i = mkSharkFind i cfgs tripState
                tripState' = tripState { sharkFinds = sharkFind <$> [1..n] }
        build "lab-management-top"  = Just $ doTransition (initLabTopState gd) cfgs gr
        build "lab-fundraiser-top"  = Just $ doTransition (initFundraiserTopState gd) cfgs gr
        build "lab-donor-list"      = Just $ doTransition (initDonorList gd) cfgs gr
        build "lab-fundraising"     = Just $ doTransition (initFundraisingState gd) cfgs gr
        build "lab-fleet-management"= Just $ doTransition (initFleetManagementState gd) cfgs gr
        build "lab-boat-store"       = Just $ doTransition (initBoatStoreState gd) cfgs gr
        build "lab-equip-management" = Just $ doTransition (initEquipManagementState gd) cfgs gr
        build "lab-equip-store"      = Just $ doTransition (initEquipStoreState gd) cfgs gr
        build "data-review-top"           = Just $ doTransition (initTopReviewState gd) cfgs gr
        build "data-review-sharks"        = Just $ doTransition (initSharkReviewTopState gd) cfgs gr
        build "data-review-shark"         = Just $ doTransition (initSharkReviewState gd sharkEntry) cfgs gr
            where
                sharkKeys  = M.keys $ gameDataFoundSharks gd
                fallback   = head $ M.keys $ sharks $ sharkCfgs cfgs
                sharkEntry = getEntry (sharks $ sharkCfgs cfgs) $ if null sharkKeys then fallback else head sharkKeys
        build "data-review-research"      = Just $ doTransition (initResearchReviewState gd) cfgs gr
        build "data-review-open-research" = Just $ doTransition (initOpenResearchState gd) cfgs gr
        build "data-review-completed-research" = Just $ doTransition (initCompletedResearchState gd) cfgs gr
        build "data-review-investigate"   = Just $ doTransition (initSubmitResearchState gd researchEntry) cfgs gr
            where researchEntry = head $ getKnownResearch (sharkCfgs cfgs) gd
        build "data-review-award-grant"   = Just $ doTransition (initAwardGrantState gd researchEntry) cfgs gr
            where researchEntry = head $ getKnownResearch (sharkCfgs cfgs) gd
        build "data-review-completed-review" = Just $ doTransition (initCompletedResearchReviewState gd researchEntry) cfgs gr
            where
                completed = filter (\r -> M.member (entryKey r) (gameDataResearchComplete gd)) $ getKnownResearch (sharkCfgs cfgs) gd
                researchEntry = head $ if null completed then getKnownResearch (sharkCfgs cfgs) gd else completed
        build _                           = Nothing
