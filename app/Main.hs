module Main where

import Env (initAppEnvData, runAppEnv)
import Configs (initConfigs, GameConfigs, lastSaveM, stateCfgs)
import OutputHandles (initOutputHandles)
import OutputHandles.Types (OutputHandles)
import Graphics (initGraphics)
import Graphics.Types (Graphics)
import InputState (initInputState, InputState)
import SaveData (loadFromFile)
import GameState.Types (GameState(..), anyDraw)
import GameState.Menu.GameMenus (initialMainMenuState)
import Game (runGame)


-- todo: Instead of this have a splash screen that doesn't need any file reads

mkInitialState :: GameConfigs -> OutputHandles -> Graphics -> InputState -> IO GameState
mkInitialState cfgs outs gr inputs = do
    gdM <- case lastSaveM (stateCfgs cfgs) of
                Nothing -> return Nothing
                Just sf -> do
                    gdE <- loadFromFile sf
                    case gdE of
                        Left err -> do
                            putStrLn $ show err
                            return Nothing
                        Right gd -> return $ Just gd
    let mmState = initialMainMenuState gdM
        gv = anyDraw mmState gr cfgs
    return $ GameState mmState gv Nothing

main :: IO ()
main = do
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    gr <- initGraphics tm outs
    inputs <- initInputState
    let env = initAppEnvData configs outs gr inputs
    gs <- mkInitialState configs outs gr inputs
    runAppEnv env $ runGame gs
