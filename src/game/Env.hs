{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env
    ( initAppEnvData
    , runAppEnv
    ) where


import Env.Types ( AppEnv(..), AppEnvData(AppEnvData) )
import Configs ( TextureCfg, GameConfigs )
import OutputHandles.Types ( OutputHandles )
import InputState ( initInputState )
import GameState ( initGameState )

import Control.Monad.Reader     (runReaderT)


initAppEnvData :: GameConfigs -> OutputHandles -> InputState -> Graphics -> GameState -> AppEnvData
initAppEnvData = AppEnvData


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = runReaderT appEnv appEnvData
