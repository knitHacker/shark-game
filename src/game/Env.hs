{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env
    ( initAppEnvData
    , runAppEnv
    ) where


import Env.Types ( AppEnv(..), AppEnvData(AppEnvData) )
import Configs ( TextureCfg, GameConfigs, stateCfgs, lastSaveM )
import OutputHandles.Types ( OutputHandles )
import InputState ( initInputState )
import GameState ( initGameState )
import Graphics ( initGraphics )
import SaveData ( loadFromFile )

import Control.Monad.Reader     (runReaderT)


initAppEnvData :: TextureCfg -> GameConfigs -> OutputHandles -> IO AppEnvData
initAppEnvData tm cfgs outs = do
    gr <- initGraphics tm outs
    inputs <- initInputState
    gdM <- case lastSaveM (stateCfgs cfgs) of
        Nothing -> return Nothing
        Just sf -> do
            gdE <- loadFromFile sf
            case gdE of
                Left err -> do
                    putStrLn err
                    return Nothing
                Right gd -> return $ Just gd
    return $ AppEnvData cfgs outs gr inputs $ initGameState gdM gr


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = runReaderT appEnv appEnvData
