{-# LANGUAGE OverloadedStrings #-}

module OutputHandles.Util
    ( renderEmpty
    , getRenderMinY
    , getRenderMaxY
    , getRenderMinX
    , getRenderMaxX
    , moveRenderY
    , clipYRender
    , mkRect
    ) where

import Foreign.C.Types ( CInt )
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import qualified SDL

import OutputHandles.Types
import OutputHandles.Text

import Debug.Trace

mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

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
        getMaxY (DrawTextDisplay td) = fromIntegral $ wordsPosY td + ceiling (heightTextDisplay fs td)

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
        getMaxR (DrawTextDisplay td) = fromIntegral $ wordsPosX td + ceiling (widthTextDisplay fs td)

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
            | wordsPosY td + textH < startY' = Nothing
            | wordsPosY td < startY' && wordsPosY td + textH < endY' =
                let xStart = wordsPosX td
                    maskWidth = ceiling $ widthTextDisplay fs td
                    maskHeight = wordsPosY td + textH - startY'
                in Just $ DrawTextDisplay $ td { wordsMask = Just (mkRect xStart startY' maskWidth maskHeight) }
            | wordsPosY td < startY' = Nothing
            | wordsPosY td < endY' && wordsPosY td + textH > endY' =
                let xStart = wordsPosX td
                    yStart = wordsPosY td
                    maskWidth = ceiling $ widthTextDisplay fs td
                    spaceH = endY' - wordsPosY td
                    maskHeight = min textH spaceH
                in Just $ DrawTextDisplay $ td { wordsMask = Just (mkRect xStart yStart maskWidth maskHeight) }
            | wordsPosY td + ceiling (heightTextDisplay fs td) > endY' = Nothing
            | otherwise = Just d
            where
                textH = ceiling (heightTextDisplay fs td)
