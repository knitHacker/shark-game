{-# LANGUAGE OverloadedStrings #-}

module OutputHandles.Util
    ( renderEmpty
    , getRenderMinY
    , getRenderMaxY
    , getRenderMinX
    , getRenderMaxX
    , moveRenderY
    , clipYRender
    ) where

import Foreign.C.Types ( CInt )
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import OutputHandles.Types
import OutputHandles.Text


renderEmpty :: ToRender
renderEmpty = mempty

getRenderMinY :: ToRender -> Int
getRenderMinY rend = minimum $ getMinY <$> renderDraws rend
    where
        getMinY :: Draw -> Int
        getMinY (DrawTexture dt) = fromIntegral $ drawPosY dt
        getMinY (DrawRectangle dr) = fromIntegral $ rectPosY dr
        getMinY (DrawTextDisplay td) = fromIntegral $ wordsPosY td

getRenderMaxY :: FontSize -> ToRender -> Int
getRenderMaxY fs rend = maximum $ getMaxY <$> renderDraws rend
    where
        getMaxY (DrawTexture dt) = fromIntegral $ drawPosY dt + drawHeight dt
        getMaxY (DrawRectangle dr) = fromIntegral $ rectPosY dr + rectHeight dr
        getMaxY (DrawTextDisplay td) = fromIntegral $ wordsPosY td + fromIntegral (heightTextDisplay fs td)

getRenderMinX :: ToRender -> Int
getRenderMinX rend = minimum $ getMinR <$> renderDraws rend
    where
        getMinR (DrawTexture dt) = fromIntegral $ drawPosX dt
        getMinR (DrawRectangle dr) = fromIntegral $ rectPosX dr
        getMinR (DrawTextDisplay td) = fromIntegral $ wordsPosX td

getRenderMaxX :: FontSize -> ToRender -> Int
getRenderMaxX fs rend = maximum $ getMaxR <$> renderDraws rend
    where
        getMaxR (DrawTexture dt) = fromIntegral $ drawPosX dt + drawWidth dt
        getMaxR (DrawRectangle dr) = fromIntegral $ rectPosX dr + rectWidth dr
        getMaxR (DrawTextDisplay td) = fromIntegral $ wordsPosX td + fromIntegral (widthTextDisplay fs td)

moveRenderY :: ToRender -> Int -> ToRender
moveRenderY r 0 = r
moveRenderY rend off = mapDraws move rend
    where
        move (DrawTexture dt) = DrawTexture $ dt { drawPosY = drawPosY dt + fromIntegral off }
        move (DrawRectangle dr) = DrawRectangle $ dr { rectPosY = rectPosY dr + fromIntegral off }
        move (DrawTextDisplay td) = DrawTextDisplay $ td { wordsPosY = wordsPosY td + fromIntegral off }

clipYRender :: FontSize -> ToRender -> Int -> Int -> ToRender
clipYRender fs rends startY endY = filterDraws clip rends
    where
        startY' = fromIntegral startY
        endY' = fromIntegral endY
        clip d@(DrawTexture dt)
            | drawPosY dt < startY' = Nothing
            | drawPosY dt + drawHeight dt > endY' = Nothing
            -- todo: need to update / add mask to only show partial image
            | otherwise = Just d
        clip d@(DrawRectangle dr)
            | rectPosY dr < startY' = Nothing
            | rectPosY dr + rectHeight dr > endY' = Nothing
            -- Can I even cut off a rect?
            | otherwise = Just d
        clip d@(DrawTextDisplay td)
            | wordsPosY td < startY' = Nothing
            | wordsPosY td + fromIntegral (heightTextDisplay fs td) > endY' = Nothing
            | otherwise = Just d
