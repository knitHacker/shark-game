{-# LANGUAGE Strict #-}
module OutputHandles.Draw
    ( mkRect
    , fillRectangle
    , mkPoint
    , setColor
    , drawLine
    , drawAll
    , initWindow
    , scale
    , unscale
    ) where


import Foreign.C.Types ( CInt )
import qualified SDL
import SDL.Vect ()
import SDL                    (($=))
import qualified SDL.Font as Font
import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Configs
import OutputHandles.Types
import OutputHandles.Util


fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

drawLine :: (MonadIO m) => SDL.Renderer -> (CInt, CInt) -> (CInt, CInt) -> m ()
drawLine r (ox, oy) (tx, ty) =
  SDL.drawLine r (mkPoint ox oy) (mkPoint tx ty)

color :: Color -> Font.Color
color White  = SDL.V4 maxBound maxBound maxBound maxBound
color Gray   = SDL.V4 (div maxBound 2) (div maxBound 2) (div maxBound 2) (div maxBound 2)
color DarkGray = SDL.V4 (div maxBound 3) (div maxBound 3) (div maxBound 3) (div maxBound 3)
color LightGray = SDL.V4 (maxBound - div maxBound 4) (maxBound - div maxBound 4) (maxBound - div maxBound 4) (maxBound - div maxBound 4)
color Black  = SDL.V4 0 0 0 0
color Red    = SDL.V4 maxBound 0 0 maxBound
color Green  = SDL.V4 0 maxBound 0 maxBound
color Blue   = SDL.V4 0 0 maxBound maxBound
color DarkBlue = SDL.V4 0 0 (div maxBound 3) (div maxBound 3)
color Yellow = SDL.V4 maxBound maxBound 0 maxBound
color (OtherColor c) = c


setColor :: (MonadIO m) => SDL.Renderer -> Color -> m ()
setColor r c = SDL.rendererDrawColor r $= color c


initWindow :: (MonadIO m) => SDL.Renderer -> m ()
initWindow r = do
    setColor r Black
    SDL.clear r
    SDL.present r


drawAll :: (MonadIO m, OutputRead m, ConfigsRead m) => ToRender -> m ()
drawAll drawings = do
    topCfgs <- readConfigs
    outs <- getOutputs
    let cfgs = settingCfgs topCfgs
        drawOutline = debugOutlineTexture cfgs
        drawDbgs = debugHitboxes cfgs
        debugs = renderDebugs drawings
        r = renderer outs
        drawings' = renderDraws drawings
        --debugs' = scaleDebugs ratX ratY <$> debugs
    SDL.clear r
    setColor r Red
    mapM_ (draw (textures outs) (font outs) drawOutline r) drawings'
    setColor r Yellow
    when drawDbgs $ mapM_ (drawDebug r) debugs
    setColor r Black
    SDL.present r

drawDebug :: MonadIO m => SDL.Renderer -> (Int, Int, Int, Int) -> m ()
drawDebug r (x, y, w, h) = do
    SDL.drawRect r (Just (mkRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)))

drawText :: MonadIO m => SDL.Renderer -> Font.Font -> TextDisplay -> m ()
drawText r font wd = do
    surf <- Font.solid font (color (wordsColor wd)) (wordsText wd)
    text <- SDL.createTextureFromSurface r surf
    (w, h) <- Font.size font (wordsText wd)
    SDL.rendererClipRect r $= wordsMask wd
    SDL.copy r text Nothing (Just (mkRect (wordsPosX wd) (wordsPosY wd) (fromIntegral (w * textScale)) (fromIntegral (h * textScale))))
    SDL.rendererClipRect r $= Nothing
    SDL.freeSurface surf
    SDL.destroyTexture text
    where
        textScale = wordsSize wd

draw :: MonadIO m => TextureMap -> Font.Font -> Bool -> SDL.Renderer -> Draw -> m ()
draw tm _ showRect r (DrawTexture dt) = drawTextureNow tm showRect r dt
draw _ _ showRect r (DrawRectangle dr) = drawRectangleNow showRect r dr
draw _ f showRect r (DrawTextDisplay td) = drawText r f td
draw tm _ showRect r (DrawAnimFrame daf) = drawAnimNow tm showRect r daf

drawAnimNow :: MonadIO m => TextureMap -> Bool -> SDL.Renderer -> DrawAnimFrame -> m ()
drawAnimNow textMap showRect r d = do
    SDL.copy r texture (Just frameMask) $ Just pos
    when showRect $ SDL.drawRect r $ Just pos
    where
        w = drawAWidth d
        h = drawAHeight d
        frameMask = mkRect (w * fromIntegral (drawFrameNum d)) (h * fromIntegral (drawDepth d)) w h
        pos = mkRect (drawPosAX d) (drawPosAY d) (scale w (drawScale d)) (scale h (drawScale d))
        texture = textMap ! drawATexture d

drawTextureNow :: MonadIO m => TextureMap -> Bool -> SDL.Renderer -> DrawTexture -> m ()
drawTextureNow texMap showRect r d = do
    SDL.copy r texture (drawMask d) $ Just pos
    when showRect $ SDL.drawRect r $ Just pos
    where
        pos = mkRect (drawPosX d) (drawPosY d) (drawWidth d) (drawHeight d)
        texture = texMap ! drawTexture d

drawRectangleNow :: MonadIO m => Bool -> SDL.Renderer -> DrawRectangle -> m ()
drawRectangleNow showRect r d = do
    setColor r (rectColor d)
    SDL.fillRect r (Just pos)
    setColor r Red
    when showRect $ SDL.drawRect r (Just pos)
    where
        pos = mkRect (rectPosX d) (rectPosY d) (rectWidth d) (rectHeight d)


unscale :: (Integral a, RealFrac b) => a -> b -> a
unscale 0 _ = 0
unscale _ 0 = 0
unscale o r = floor $ fromIntegral o / r

scale :: (Integral a, RealFrac b) => a -> b -> a
scale 0 _ = 0
scale _ 0 = 0
scale o r = floor (fromIntegral o * r)

scaleDraw :: Double -> Double -> Draw -> Draw
scaleDraw rX rY (DrawTexture (DTexture t pX pY w h m)) = DrawTexture $ DTexture t (scale pX rX) (scale pY rY) (scale w rX) (scale h rY) m
scaleDraw rX rY (DrawRectangle (DRectangle t pX pY w h)) = DrawRectangle $ DRectangle t (scale pX rX) (scale pY rY) (scale w rX) (scale h rY)
scaleDraw rX rY (DrawTextDisplay td) = DrawTextDisplay $ scaleWords rX rY td

scaleWords :: Double -> Double -> TextDisplay -> TextDisplay
scaleWords rX rY (TextDisplay wd pX pY s c mm) = TextDisplay wd (scale pX rX) (scale pY rY) s c mm

scaleDebugs :: Double -> Double -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
scaleDebugs rX rY (x, y, w, h) = (scale x rX, scale y rY, scale w rX, scale h rY)
