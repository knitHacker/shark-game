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

initGameState :: GameConfigs -> OutputHandles -> IO GameState
initGameState cfgs outs = do
    gv <- mainMenuView cfgs outs
    return $ reDraw gv

isGameExiting :: GameState -> Bool
isGameExiting (GameState GameExiting _ _) = True
isGameExiting _ = False

updateGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameState = do
    cfgs <- readConfigs
    inputs <- readInputState
    gs <- readGameState
    outs <- getOutputs
    gsM <- case gameView gs of
        GameView mm m -> return $ updateGameStateInMenu mm m cfgs inputs outs
        OverlayMenu tm bm -> return $ updateGameStateInOverlay tm bm cfgs inputs outs
        _ -> return Nothing
    case gsM of
        Nothing -> return $ noUpdate gs
        Just (Left gv) -> return $ reDraw gv
        Just (Right gps) -> do
            gv <- liftIO $ moveToNextState gps cfgs inputs outs
            return $ reDraw gv


moveToNextState :: GamePlayState -> GameConfigs -> InputState -> OutputHandles -> IO GameView
moveToNextState gps cfgs inputs outs =
    case gps of
        GameExitState (Just gd) -> do
            saveGame gd cfgs
            return GameExiting
        GameExitState Nothing -> return GameExiting
        MainMenu gd -> do
            saveGame gd cfgs
            return $ GameView Nothing $ mainMenu (Just gd) cfgs outs
        IntroPage -> introPageIO
        ResearchCenter gd -> return $ withPause gps gd $ researchCenterMenu gd outs
        TripDestinationSelect gd -> return $ withPause gps gd $ mapMenu gd cfgs
        TripEquipmentSelect gd loc eqs cp -> return $ withPause gps gd $ equipmentPickMenu gd loc eqs cp cfgs
        TripReview gd loc eqs -> return $ withPause gps gd $ reviewTripMenu gd loc eqs cfgs
        TripProgress gd tp -> return $ withPause gps gd $ tripProgressMenu gd tp cfgs
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
    return $ GameView Nothing $ mainMenu gdM cfgs outs


introPageIO :: IO GameView
introPageIO = do
    nGame <- startNewGame
    return $ GameView Nothing $ introPage nGame
