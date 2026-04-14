{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

module Env.Types
    ( AppEnvData(..)
    , AppEnv(..)
    ) where

import Configs
import OutputHandles.Types
import OutputHandles
import InputState
import Data.Word (Word32)
import GameState.Types
import Graphics.Types (GraphicsRead(..))
import Graphics.Types
import SaveData

import qualified Data.Text as T
import Control.Monad.State.Strict (StateT, MonadState, gets, modify)
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


newtype AppEnv a = AppEnv (StateT AppEnvData IO a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState AppEnvData
        , MonadIO
        )


instance GraphicsRead AppEnv where
    readGraphics :: AppEnv Graphics
    readGraphics = gets appEnvDataGraphics

    resizeGraphics :: (Int, Int) -> AppEnv ()
    resizeGraphics (w, h) = modify $ \d ->
        let gr = appEnvDataGraphics d
        in d { appEnvDataGraphics = gr { graphicsWindowWidth = w, graphicsWindowHeight = h } }

instance InputRead AppEnv where
    readInputState :: AppEnv InputState
    readInputState = gets appEnvDataInputState

    -- also stores the new InputState back into AppEnvData
    pollInputState :: Word32 -> AppEnv InputResult
    pollInputState timeout = do
        res <- updateInput timeout
        updateInputState (newInputs res)
        return res

    updateInputState :: InputState -> AppEnv ()
    updateInputState inputs = modify $ \d -> d { appEnvDataInputState = inputs }

instance ConfigsRead AppEnv where
    readConfigs :: AppEnv GameConfigs
    readConfigs = gets appEnvDataConfigs


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

instance RendererActions AppEnv where
    getFontSize :: AppEnv FontSize
    getFontSize = gets appEnvDataOutputHandles >>= getOutputFontSize

    getWindowSize :: AppEnv (Int, Int)
    getWindowSize = gets appEnvDataOutputHandles >>= liftIO . getOutputWindowSize

    cleanupRenderer :: AppEnv ()
    cleanupRenderer = gets appEnvDataOutputHandles >>= liftIO . cleanupOutputHandles

    executeDraw :: ToRender -> AppEnv ()
    executeDraw toRender = do
        outs <- gets appEnvDataOutputHandles
        renderFrame outs toRender

instance GameStateStep AppEnv where
    getUpdate :: InputResult -> GameState -> AppEnv Update
    getUpdate inputRes gs = do
        cfgs <- readConfigs
        gr   <- readGraphics
        return $ anyThink (gameCurrentState gs) cfgs inputRes gr

    executeAction :: Update -> AppEnv (Maybe Step)
    executeAction Exit                = return Nothing
    executeAction (PureStep step)     = return $ Just step
    executeAction (GenerateNewGame f) = do
        cfgs <- readConfigs
        gd   <- liftIO $ startNewGame cfgs
        return $ Just (f gd)
    executeAction (SaveFile gd step)  = do
        liftIO $ saveToFile gd
        return $ Just step
    executeAction (LoadFile fp f)     = do
        result <- liftIO $ loadFromFile fp
        return $ Just (f result)
    executeAction (SaveList f)        = do
        fps <- getSaveFiles
        return $ Just (f fps)

    stepGame :: GameState -> Step -> AppEnv (Maybe GameState)
    stepGame _  UseCache            = return Nothing
    stepGame _  (UpdateTo next)     = do
        gr   <- readGraphics
        cfgs <- readConfigs
        return $ Just $ GameState next (anyDraw next gr cfgs) Nothing
    stepGame _  (TransitionTo next) = do
        gr   <- readGraphics
        cfgs <- readConfigs
        return $ Just $ anyInitialize next gr cfgs
