{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
module GameState
    ( initGameState
    , isGameExiting
    , updateGameState
    ) where

import Control.Monad ()
import Configs ( ConfigsRead(readConfigs), GameConfigs )
import InputState
import GameState.Types
import OutputHandles.Types ( OutputHandles, OutputRead(..) )
import GameState.Menu
import GameState.Menu.GameMenus
import GameState.Menu.TripMenus
import SaveData
import Configs

import qualified Data.Text as T
import qualified SDL

import qualified SDL.Image
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Control.Monad.IO.Class ( MonadIO(..) )

import Debug.Trace

initGameState :: GameConfigs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    gv <- mainMenuView cfgs outs
    return $ reDraw gv

isGameExiting :: GameState -> Bool
isGameExiting (GameState GameExiting _) = True
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
        Just (Left gv) -> return $ reDraw gv
        Just (Right gps) -> do
            gv <- liftIO $ moveToNextState gps cfgs inputs outs
            return $ reDraw gv


updateGameTimeout :: (Maybe OverlayMenu) -> TimeoutView -> InputState -> Maybe (Either GameView GamePlayState)
updateGameTimeout mM tv i@(InputState _ ts) =
    case (esc, mM, to) of
        (True, Just om, _) -> Just $ Left $ OverlayMenu om $ BasicTimeoutView tv
        (_, _, True) -> Just $ Right $ timeoutAction tv
        _ -> Nothing
    where
        esc = escapeJustPressed i
        to = ts - (lastTimeout tv) > timeoutLength tv

moveToNextState :: GamePlayState -> GameConfigs -> InputState -> OutputHandles -> IO GameView
moveToNextState gps cfgs inputs outs =
    case gps of
        GameExitState (Just gd) -> do
            saveGame gd cfgs
            return GameExiting
        GameExitState Nothing -> return GameExiting
        MainMenu gd -> do
            saveGame gd cfgs
            return $ GameMenu Nothing $ mainMenu (Just gd) cfgs outs
        IntroPage -> introPageIO
        ResearchCenter gd -> return $ withPause gps gd $ BasicMenu $ researchCenterMenu gd outs
        TripDestinationSelect gd -> return $ withPause gps gd $ BasicMenu $ mapMenu gd cfgs
        TripEquipmentSelect gd loc eqs cp -> return $ withPause gps gd $ BasicMenu $ equipmentPickMenu gd loc eqs cp cfgs
        TripReview gd loc eqs -> return $ withPause gps gd $ BasicMenu $ reviewTripMenu gd loc eqs cfgs
        TripProgress gd tp -> return $ withPause gps gd $ BasicTimeoutView $ tripProgressMenu gd tp cfgs inputs
        SharkFound gd sf tp -> return $ GameMenu Nothing $ sharkFoundMenu gd sf tp cfgs
        TripResults gd tp -> return $ GameMenu Nothing $ tripResultsMenu gd tp cfgs
        _ -> undefined


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
    return $ GameMenu Nothing $ mainMenu gdM cfgs outs


introPageIO :: IO GameView
introPageIO = do
    nGame <- startNewGame
    return $ GameMenu Nothing $ introPage nGame
