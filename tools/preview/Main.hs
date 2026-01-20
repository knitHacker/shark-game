module Main where

import Control.Monad (unless)
import System.Environment (getArgs)

import Configs
import Env
import GameState
import GameState.Types
import Graphics
import Graphics.Types
import InputState
import OutputHandles
import SaveData
import SaveData.Generate

main :: IO ()
main = do
    args <- getArgs
    let targetState = case args of
            (s:_) -> read s
            []    -> ResearchCenter

    (outs, cfgs) <- initConfigs
    gr <- initGraphics cfgs outs
    gd <- defaultGameData cfgs
    let (lgps, gv) = initializeState targetState cfgs gr gd
        gs = GameState gr lgps gv Nothing

    previewLoop outs cfgs gs

previewLoop :: OutputHandles -> GameConfigs -> GameState -> IO ()
previewLoop outs cfgs gs = do
    is <- pollInputs outs
    unless (isQuit is) $ do
        let env = Env cfgs gs is outs
        gs' <- runEnv env updatePreviewGameState
        drawGameState outs cfgs gs'
        previewLoop outs cfgs gs'

updatePreviewGameState :: (MonadIO m, ConfigsRead m, GameStateRead m, InputRead m, OutputRead m)
                       => m GameState
updatePreviewGameState = updateGameStateWith BlockTransitions

