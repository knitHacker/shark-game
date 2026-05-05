{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, gets, modify)
import System.Environment (getArgs)
import qualified Data.Text as T
import Data.Word (Word32)

import Configs (initConfigs, ConfigsRead(..), GameConfigs)
import OutputHandles (initOutputHandles, executeDraw, cleanupOutputHandles)
import OutputHandles.Types (OutputHandles, OutputRead(..), RenderAction(..), ToRender)
import Graphics (initGraphics)
import Graphics.Types (Graphics(..), GraphicsRead(..), GraphicsUpdate(..))
import Graphics.NewDraw (drawAssets)
import GameState (stepGameState)
import GameState.Types
import GameState.Menu.GameMenus
import GameState.Menu.TripMenus
import InputState
import SaveData (GameData, loadFromFile)
import Generate (defaultGameData)
import Game (runGame)

data PreviewEnvData = PreviewEnvData
    { previewConfigs       :: !GameConfigs
    , previewOutputHandles :: !OutputHandles
    , previewInputState    :: !InputState
    , previewGraphics      :: !Graphics
    , previewGameState     :: !GameStateNew
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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: preview <state-name> [<save-file>]"
        (stateName:rest) -> do
            (tm, cfgs) <- initConfigs
            outs <- initOutputHandles tm cfgs
            gr <- initGraphics tm outs
            gd <- loadGameData cfgs rest
            case buildInitialState stateName cfgs gr gd of
                Nothing  -> putStrLn $ "Unknown state: " ++ stateName ++ "\nAvailable: main-menu, research-center"
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

buildInitialState :: String -> GameConfigs -> Graphics -> GameData -> Maybe GameStateNew
buildInitialState "main-menu"       cfgs gr gd = Just $ transition (MainMenuState (Just gd) ContinueMain []) cfgs gr
buildInitialState "intro-welcome"   cfgs gr gd = Just $ transition (IntroState gd IntroWelcomePage) cfgs gr
buildInitialState "intro-mission"   cfgs gr gd = Just $ transition (IntroState gd IntroMissionPage) cfgs gr
buildInitialState "intro-boat"      cfgs gr gd = Just $ transition (IntroState gd IntroBoatPage) cfgs gr
buildInitialState "intro-equip"     cfgs gr gd = Just $ transition (IntroState gd IntroEquipPage) cfgs gr
buildInitialState "intro-research"  cfgs gr gd = Just $ transition (IntroState gd IntroResearchPage) cfgs gr
buildInitialState "intro-funds"     cfgs gr gd = Just $ transition (IntroState gd IntroFundsPage) cfgs gr
buildInitialState "intro-end"       cfgs gr gd = Just $ transition (IntroState gd IntroEndPage) cfgs gr
buildInitialState "research-center" cfgs gr gd = Just $ transition (initResearchCenter gd) cfgs gr
buildInitialState "trip-map"        cfgs gr gd = Just $ transition (initTripMapState gd) cfgs gr
buildInitialState "trip-equip"      cfgs gr gd = Just $ transition (initEquipPickState gd 1 "estuary") cfgs gr
buildInitialState _                 _    _  _  = Nothing
