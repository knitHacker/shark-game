module OutputHandles.Images
    ( addRectangle
    , addTexture
    , getRectMinY
    , getRectMinX
    , getRectMaxY
    , getRectMaxX
    , getImageMinY
    , getImageMinX
    , getImageMaxY
    , getImageMaxX
    ) where

import OutputHandles.Types

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

addRectangle :: ToRender -> Int -> Int -> DrawRectangle -> ToRender
addRectangle rend depth priority dr = addDraw rend depth priority (DrawRectangle dr)

addTexture :: ToRender -> Int -> Int -> DrawTexture -> ToRender
addTexture rend depth priority dt = addDraw rend depth priority (DrawTexture dt)
