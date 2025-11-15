{-# LANGUAGE Strict #-}
{-# LANGUAGE InstanceSigs #-}
module OutputHandles.Types
    ( OutputHandles(..)
    , OutputRead(..)
    , Draw(..)
    , Color(..)
    , Draws
    , ToRender
    , TextDisplay(..)
    , TextureMap
    , DrawRectangle(..)
    , DrawTexture(..)
    , FontSize
    , Image
    , lengthDraws
    , renderDebugs
    , renderDraws
    , mapDraws
    , filterDraws
    , addDraw
    ) where

import Foreign.C.Types ( CInt )
import qualified SDL
import qualified SDL.Font as Font
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import SDL.Font (height)

import Debug.Trace

type FontSize = (Double, Double)

-- first number is layer number
-- third number is priority
type Position = (Int, Int, Int)

type Draws = M.Map Position Draw

type Image = T.Text

data ToRender = ToRender
    { nextId :: !Int
    , draws :: !Draws
    , drawDebugs :: ![(Int, Int, Int, Int)]
    }

lengthDraws :: ToRender -> Int
lengthDraws (ToRender _ ds _) = M.size ds

renderDebugs :: ToRender -> [(Int, Int, Int, Int)]
renderDebugs = drawDebugs

renderDraws :: ToRender -> [Draw]
renderDraws rend = M.elems $ draws rend

mapDraws :: (Draw -> Draw) -> ToRender -> ToRender
mapDraws f (ToRender n ds dbs) = ToRender n (M.map f ds) dbs

filterDraws :: (Draw -> Maybe Draw) -> ToRender -> ToRender
filterDraws f (ToRender n ds dbs) = ToRender n (M.mapMaybe f ds) dbs

addDraw :: ToRender -> Int -> Int -> Draw -> ToRender
addDraw (ToRender n ds dbs) depth priority d = ToRender (n + 1) ds' dbs
    where
        ds' = M.insert (depth, priority, n) d ds

instance Monoid ToRender where
    mempty :: ToRender
    mempty = ToRender 0 M.empty []

instance Semigroup ToRender where
   (<>) :: ToRender -> ToRender -> ToRender
   (<>) (ToRender n1 m1 d1) (ToRender n2 m2 d2) = ToRender (n1+n2) (m1 <> m2') (d1 <> d2)
    where
        m2' = M.mapKeys (\(a, b, c) -> (a, b, c + n1)) m2

data Color = White
           | Gray
           | DarkGray
           | LightGray
           | Black
           | Red
           | Blue
           | DarkBlue
           | Green
           | Yellow
           | OtherColor !Font.Color

data Draw = DrawTexture DrawTexture
          | DrawRectangle DrawRectangle
          | DrawTextDisplay TextDisplay


data DrawTexture = DTexture
    { drawTexture :: !Image -- Possible error, might not exist in the texture map
    , drawPosX :: !CInt
    , drawPosY :: !CInt
    , drawWidth :: !CInt
    , drawHeight :: !CInt
    , drawMask :: !(Maybe (SDL.Rectangle CInt))
    }

data DrawRectangle = DRectangle
    { rectColor :: !Color
    , rectPosX :: !CInt
    , rectPosY :: !CInt
    , rectWidth :: !CInt
    , rectHeight :: !CInt
    }

data TextDisplay = TextDisplay
    { wordsText :: !T.Text
    , wordsPosX :: !CInt
    , wordsPosY :: !CInt
    , wordsSize :: !Int
    , wordsColor :: !Color
    , wordsMask :: !(Maybe (SDL.Rectangle CInt))
    }

type TextureMap = M.Map Image SDL.Texture

data OutputHandles = OutputHandles
    { window :: SDL.Window
    , renderer :: SDL.Renderer
    , textures :: TextureMap
    -- Probably want this to be a map eventually because you want more than one font
    , font :: Font.Font
    }

class Monad m => OutputRead m where
    getOutputs :: m OutputHandles
