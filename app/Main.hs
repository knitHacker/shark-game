module Main where

import System.Exit (exitSuccess)

import Env (initAppEnvData)
import Configs (initConfigs)
import OutputHandles (initOutputHandles)
import Game (runGame)


mkInitialState :: IO GameState
mkInitialState = do

mkInitialState :: GameConfigs -> OutputHandles -> Graphics -> InputState -> IO GameState
mkInitialState cfgs outs gr inputs = do
    gdM <- case lastSaveM (stateCfgs cfgs) of
                Nothing -> return Nothing
                Just sf -> do
                    gdE <- loadFromFile sf
                    case gdE of
                        Left err -> do
                            putStrLn $ T.unpack err
                            return Nothing
                        Right gd -> return $ Just gd
    let mmState = initialMainMenuState gdM
        gv = draw mmState gr cfgs
    return $ GameState mmState gv Nothing

main :: IO ()
main = do
    (tm, configs) <- initConfigs
    outs <- initOutputHandles tm configs
    gr <- initGraphics tm outs
    inputs <- initInputs
    let env = initAppEnvData configs outs gr inputs
    gs <- mkInitialState configs outs gr inputs
    runGame env gs
