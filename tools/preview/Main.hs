module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import System.Environment (getArgs)

import Configs
import Env
import Env.Types
import GameState
import GameState.Types
import GameState.Draw
import Graphics
import InputState
import OutputHandles
import OutputHandles.Types
import SaveData

import Generate

main :: IO ()
main = do
    args <- getArgs
    (tm, cfgs) <- initConfigs
    outs <- initOutputHandles tm cfgs
    gr <- initGraphics tm outs
    inputs <- initInputState
    gd <- defaultGameData cfgs

    let targetState = parseTargetState args gd
        gv = reDrawState targetState cfgs inputs gr
        gs = GameState gr targetState gv Nothing
        appEnv = AppEnvData cfgs outs inputs gs

    previewLoop appEnv

parseTargetState :: [String] -> GameData -> GamePlayState
parseTargetState [] gd = ResearchCenter gd
parseTargetState (s:_) gd = case s of
    "MainMenu"            -> MainMenu (Just gd)
    "ResearchCenter"      -> ResearchCenter gd
    "DataReviewTop"       -> DataReviewTop gd
    "SharkReviewTop"      -> SharkReviewTop gd Nothing
    "ResearchReviewTop"   -> ResearchReviewTop gd
    "LabManagement"       -> LabManagement gd
    "FundraiserTop"       -> FundraiserTop gd
    "FleetManagement"     -> FleetManagement gd
    "EquipmentManagement" -> EquipmentManagement gd
    "ViewDonors"          -> ViewDonors gd
    _                     -> ResearchCenter gd

previewLoop :: AppEnvData -> IO ()
previewLoop appEnv = do
    inputs <- runAppEnv appEnv (updateInput 16)  -- ~60fps
    unless (inputQuit inputs) $ do
        let appEnv' = appEnv { appEnvDataInputState = inputs }
        gs' <- runAppEnv appEnv' updateGameStatePreview
        draws <- runAppEnv appEnv' { appEnvDataGameState = gs' } updateWindow
        runAppEnv appEnv' { appEnvDataGameState = gs' } (executeDraw draws)
        previewLoop appEnv' { appEnvDataGameState = gs' }

-- Add to GameState.hs and export
updateGameStatePreview :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m) => m GameState
updateGameStatePreview = updateGameStateWith BlockTransitions
