{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env.Types
    ( AppEnvData(..)
    , AppEnv(..)
    ) where

import Configs
import OutputHandles.Types
import InputState
import GameState.Types
import Graphics (GraphicsRead(..))
import Graphics.Types
import SaveData

import qualified Data.Text as T
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory
import System.FilePath (takeExtension)


data AppEnvData = AppEnvData
    { appEnvDataConfigs :: !GameConfigs
    , appEnvDataOutputHandles :: !OutputHandles
    , appEnvDataGraphics :: !Graphics
    , appEnvDataInputState :: !InputState
    }


newtype AppEnv a = AppEnv (ReaderT AppEnvData IO a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppEnvData
        , MonadIO
        )

instance OutputRead AppEnv where
    getOutputs :: AppEnv OutputHandles
    getOutputs = asks appEnvDataOutputHandles

instance GraphicsRead AppEnv where
    readGraphics :: AppEnv Graphics
    readGraphics = asks appEnvDataGraphics

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = asks appEnvDataInputState

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv GameConfigs
    readConfigs = asks appEnvDataConfigs

instance GameStateRead AppEnv where
    readGameState :: AppEnv GameState
    readGameState = asks appEnvDataGameState

instance GameDataStorage AppEnv where
    saveData :: GameData -> AppEnv ()
    saveData gd = liftIO $ saveToFile gd

    loadData :: FilePath -> AppEnv (Either T.Text GameData)
    loadData fp = liftIO $ loadFromFile fp

    getSaveFiles :: AppEnv [FilePath]
    getSaveFiles = do
        localPath <- liftIO $ getSaveDir
        contents <- liftIO $ listDirectory localPath
        return $ filter (\f -> takeExtension f == ".save") contents

instance GameStateStep AppEnv where
    executeThink :: AnyGamePlayState -> AppEnv Step
    executeThink agps@(AnyGamePlayState s _) = do
        cfgs <- readConfigs
        case think s cfgs of
            PureStep step          -> return step
            GenerateNewGame f      -> do
                gd <- liftIO $ startNewGame cfgs
                return $ f gd
            SaveFile gd step       -> do
                liftIO $ saveToFile gd
                return step
            LoadFile fp f          -> do
                result <- liftIO $ loadFromFile fp
                return $ f result
            SaveList f             -> do
                fps <- getSaveFiles
                return $ f fps

    resolveStep :: AnyGamePlayState -> Step -> AppEnv (Maybe AnyGamePlayState)
    resolveStep _     Exit           = return Nothing
    resolveStep state NoChange       = return $ Just state
    resolveStep _     (Transition next) = do
        step <- executeThink next
        resolveStep next step
