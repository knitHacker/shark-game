module OutputHandles.Images
    ( addRectangle
    , addTexture
    , addAnimTexture
    , getRectMinY
    , getRectMinX
    , getRectMaxY
    , getRectMaxX
    , getImageMinY
    , getImageMinX
    , getImageMaxY
    , getImageMaxX
    , getAnimMaxY
    , getAnimMaxX
    , partialTexture
    ) where

import qualified Data.Text as T

import OutputHandles.Types
import OutputHandles.Util

getRectMinY :: [DrawRectangle] -> Int
getRectMinY rects = minimum $ map (fromIntegral . rectPosY) rects

getRectMinX :: [DrawRectangle] -> Int
getRectMinX rects = minimum $ map (fromIntegral . rectPosX) rects

getRectMaxY :: [DrawRectangle] -> Int
getRectMaxY rects = maximum $ map (\r -> fromIntegral (rectPosY r + rectHeight r)) rects

getRectMaxX :: [DrawRectangle] -> Int
getRectMaxX rects = maximum $ map (\r -> fromIntegral (rectPosX r + rectWidth r)) rects

getImageMinY :: [DrawTexture] -> Int
getImageMinY imgs = minimum $ map (fromIntegral . drawPosY) imgs

getImageMinX :: [DrawTexture] -> Int
getImageMinX imgs = minimum $ map (fromIntegral . drawPosX) imgs

getImageMaxY :: [DrawTexture] -> Int
getImageMaxY imgs = maximum $ map (\img -> fromIntegral (drawPosY img + drawHeight img)) imgs

getImageMaxX :: [DrawTexture] -> Int
getImageMaxX imgs = maximum $ map (\img -> fromIntegral (drawPosX img + drawWidth img)) imgs

getAnimMaxY :: [DrawAnimFrame] -> Int
getAnimMaxY anims = maximum $ map (\anim -> fromIntegral (drawPosAY anim + ceiling (fromIntegral (drawAHeight anim) * drawScale anim))) anims

getAnimMaxX :: [DrawAnimFrame] -> Int
getAnimMaxX anims = maximum $ map (\anim -> fromIntegral (drawPosAX anim + ceiling (fromIntegral (drawAWidth anim) * drawScale anim))) anims

addRectangle :: ToRender -> Int -> Int -> DrawRectangle -> ToRender
addRectangle rend depth priority dr = addDraw rend depth priority (DrawRectangle dr)

partialTexture :: T.Text -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int) -> DrawTexture
partialTexture img x y w h (mx, my, mw, mh) =
    DTexture img (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (Just mask)
    where
        mask = mkRect (fromIntegral mx) (fromIntegral my) (fromIntegral mw) (fromIntegral mh)


addTexture :: ToRender -> Int -> Int -> DrawTexture -> ToRender
addTexture rend depth priority dt = addDraw rend depth priority (DrawTexture dt)

addAnimTexture :: ToRender -> Int -> Int -> DrawAnimFrame -> ToRender
addAnimTexture rend depth priority daf = addDraw rend depth priority (DrawAnimFrame daf)