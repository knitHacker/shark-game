module IOApi
    ( FileReader(..)
    , FileWriter(..)
    , GraphicsInfo(..)
    , InputReader(..)
    , Drawable(..)
    ) where

import qualified Data.Text as T

import GameData.Types
import GameData.Load
import Handles.Types
import Draw.Types
import Configs
import InputState.Types
import Graphics.Types

import Data.Word ( Word32 )

class FileReader m where
    reloadConfigs :: m (Either T.Text GameConfigs)
    readUserState :: m (Either T.Text StateConfig)
    readSave :: String -> m (Either T.Text GameSaveData)
    listFiles :: m (Either T.Text SaveFileInfo)
    -- readTextureInfo :: m (Either T.Text TextureFileMap)


class FileWriter m where
    writeUserState :: StateConfig -> m (Either T.Text ())
    writeSave :: GameSaveData -> m (Either T.Text ())


class FileReader m => GraphicsInfo m where
    getGraphics :: m Graphics


class InputReader m where
    getNextInput :: m InputState
    updateInput :: Word32 -> InputState -> m InputState


class Drawable m where
    draw :: ToRender -> m ()
