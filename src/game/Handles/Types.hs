{-# LANGUAGE Strict #-}
{-# LANGUAGE InstanceSigs #-}

module Handles.Types
    ( TextureMap
    , FontSize
    , Image
    , Handles(..)
    ) where

import qualified SDL
import qualified SDL.Font as Font
import qualified Data.Map.Strict as M
import qualified Data.Text as T


type FontSize = (Double, Double)

type TextureMap = M.Map Image SDL.Texture

type Image = T.Text

-- todo: Add
-- - sound
-- - controller(s)
data Handles = Handles
  { window :: SDL.Window
  , renderer :: SDL.Renderer
  , textures :: TextureMap
  , font :: Font.Font
  }
