{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env
    ( initAppEnvData
    , runAppEnv
    ) where


import Env.Types
import Configs
import OutputHandles.Types
import InputState
import GameState.Types
import Graphics.Types

import Control.Monad.State.Strict (evalStateT)

initAppEnvData :: GameConfigs -> OutputHandles -> InputState -> Graphics -> GameStateNew -> AppEnvData
initAppEnvData = AppEnvData

runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = evalStateT appEnv appEnvData