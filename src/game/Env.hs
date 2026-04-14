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
import InputState ( initInputState, InputState )
import Graphics ( initGraphics )
import Graphics.Types ( Graphics )
import SaveData ( loadFromFile )
import qualified Data.Text as T

import Control.Monad.State.Strict (evalStateT)


initAppEnvData :: GameConfigs -> OutputHandles -> Graphics -> InputState -> AppEnvData
initAppEnvData = AppEnvData


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = evalStateT appEnv appEnvData
