{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OutputHandles.Types
    ( OutputHandles(..)
    , OutputRead(..)
    , Draw(..)
    , Color(..)
    , TextureEntry(..)
    , Draws
    , ToRender
    , TextDisplay(..)
    , TextureMap
    , DrawRectangle(..)
    , DrawTexture(..)
    , renderEmpty
    , renderDebugs
    , renderDraws
    , addTexture
    , addRectangle
    , addText
    ) where

import Foreign.C.Types ( CInt )
import qualified SDL
import qualified SDL.Font as Font
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- first number is layer number
-- third number is priority
type Position = (Int, Int, Int)

type Draws = M.Map Position Draw

data ToRender = ToRender
    { nextId :: !Int
    , draws :: !Draws
    , drawDebugs :: ![(Int, Int, Int, Int)]
    }

instance Monoid ToRender where
    mempty :: ToRender
    mempty = ToRender 0 M.empty []

instance Semigroup ToRender where
   (<>) :: ToRender -> ToRender -> ToRender
   (<>) (ToRender n1 m1 d1) (ToRender n2 m2 d2) = ToRender (n1+n2) (m1 <> m2') (d1 <> d2)
    where
        m2' = M.mapKeys (\(a, b, c) -> (a, b, c + n1)) m2

data Color = White | Gray | Black | Red | Blue | Green | Yellow

data Draw = DrawTexture DrawTexture
          | DrawRectangle DrawRectangle
          | DrawTextDisplay TextDisplay

renderEmpty :: ToRender
renderEmpty = ToRender 0 M.empty []

renderDebugs :: ToRender -> [(Int, Int, Int, Int)]
renderDebugs = drawDebugs

renderDraws :: ToRender -> [Draw]
renderDraws rend = M.elems $ draws rend


data DrawTexture = DTexture
    { drawTexture :: SDL.Texture
    , drawPosX :: CInt
    , drawPosY :: CInt
    , drawWidth :: CInt
    , drawHeight :: CInt
    , drawMask :: Maybe (SDL.Rectangle CInt)
    }

data DrawRectangle = DRectangle
    { rectColor :: Color
    , rectPosX :: CInt
    , rectPosY :: CInt
    , rectWidth :: CInt
    , rectHeight :: CInt
    }

data TextDisplay = TextDisplay
    { wordsText :: T.Text
    , wordsPosX :: CInt
    , wordsPosY :: CInt
    , wordsWidth :: CInt
    , wordsHeight :: CInt
    , wordsColor :: Color
    }


data TextureEntry = TextureEntry
    { textureWidth :: Int
    , textureHeight :: Int
    , texture :: SDL.Texture
    }

addTexture :: ToRender -> Int -> Int -> DrawTexture -> ToRender
addTexture (ToRender n ds dbs) depth priority dt = ToRender (n + 1) ds' dbs
    where
        ds' = M.insert (depth, priority, n) (DrawTexture dt) ds

addRectangle :: ToRender -> Int -> Int -> DrawRectangle -> ToRender
addRectangle (ToRender n ds dbs) depth priority dr = ToRender (n + 1) ds' dbs
    where
        ds' = M.insert (depth, priority, n) (DrawRectangle dr) ds

addText :: ToRender -> Int -> Int -> TextDisplay -> ToRender
addText (ToRender n ds dbs) depth priority td = ToRender (n + 1) ds' dbs
    where
        ds' = M.insert (depth, priority, n) (DrawTextDisplay td) ds

type TextureMap = M.Map T.Text TextureEntry

data OutputHandles = OutputHandles
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , textures :: TextureMap
    , font :: Font.Font
    , ratioX :: Double
    , ratioY :: Double
    }


class Monad m => OutputRead m where
    getOutputs :: m OutputHandles
