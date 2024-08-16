{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env
    ( initAppEnvData
    , runAppEnv
    ) where


import Env.Types ( AppEnv(..), AppEnvData(AppEnvData) )
import Configs ( GameConfigs )
import OutputHandles.Types ( OutputHandles )
import InputState ( initInputState )
import GameState ( initGameState )

import Control.Monad.Reader     (runReaderT)


initAppEnvData :: GameConfigs -> OutputHandles -> IO AppEnvData
initAppEnvData cfgs outs = do
    inputs <- initInputState
    game <- initGameState cfgs outs
    return $ AppEnvData cfgs outs inputs game


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = runReaderT appEnv appEnvData
