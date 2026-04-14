module Main where

import Env (initAppEnvData, runAppEnv)
import Configs (initConfigs, GameConfigs, lastSaveM, stateCfgs)
import OutputHandles (initOutputHandles)
import OutputHandles.Types (OutputHandles)
import Graphics (initGraphics)
import Graphics.Types (Graphics)
import InputState (initInputState, InputState(..))
import SaveData (loadFromFile)
import GameState.Types (GameState(..), AnyGamePlayState(..), anyInitialize)
import GameState.Menu.GameMenus (SplashScreenState(..))
import Game (runGame)


-- todo: Instead of this have a splash screen that doesn't need any file reads

mkInitialState :: GameConfigs -> Graphics -> InputState -> GameState
mkInitialState cfgs gr inputs = anyInitialize splash gr cfgs
    where
        splash = AnyGamePlayState (SplashScreen (timestamp inputs)) False

main :: IO ()
main = do
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    gr <- initGraphics tm outs
    inputs <- initInputState
    let env = initAppEnvData configs outs gr inputs
        gs = mkInitialState configs gr inputs
    runAppEnv env $ runGame gs
