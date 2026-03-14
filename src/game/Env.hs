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

initConfigs :: IO (Either T.Text (TextureCfg, GameConfigs))
initConfigs = do
    gPath <- getGameFullPath configFile
    configsE <- decodeYaml gPath
    tPath <- getGameFullPath texturesFile
    texturesE <- decodeYaml tPath
    pPath <- getGameFullPath sharkFile
    rPath <- getGameFullPath researchFile
    mPath <- getGameFullPath mechanicsFile
    gamePlayE <- decodeYaml pPath :: IO (Either String GamePlayConfig)
    researchE <- decodeYaml rPath :: IO (Either String ResearchConfig)
    mechanicsE <- decodeYaml mPath :: IO (Either String GameMechanicsConfig)
    let playConfigsE = buildPlayConfigs <$> gamePlayE <*> researchE <*> mechanicsE
    let cfgE = Right Configs <*> texturesE <*> configsE <*> playConfigsE
    case cfgE of
        Left err -> error ("Faile to parse game configs: " ++ show err)
        Right configs -> if checkConfigs configs
                            then return (textureCfgs configs, toGameConfigs configs)
                            else error "Invalid game configs"
  where
    buildPlayConfigs :: GamePlayConfig -> ResearchConfig -> GameMechanicsConfig -> PlayConfigs
    buildPlayConfigs (GamePlayConfig b e r s) res (GameMechanicsConfig gm) = PlayConfigs
        { boats = b
        , equipment = e
        , regions = r
        , sharks = s
        , research = res
        , gameMechanics = gm
        }

initAppEnv :: TextureCfg -> GameConfigs -> OutputHandles -> IO AppEnvData
initAppEnv tm cfgs outs = do
    inputs <- initInputState
    game <- initGameState tm cfgs outs inputs
    return $ AppEnvData cfgs outs inputs game


runAppEnv :: AppEnvData -> AppEnv a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = runReaderT appEnv appEnvData
