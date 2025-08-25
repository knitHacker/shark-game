{-# LANGUAGE OverloadedStrings #-}

module Graphics.TextUtil
    ( fontWidth
    , fontHeight
    , wrapText
    , wrapTexts
    , adjustTexts
    , oneLine
    ) where

import qualified Data.Text as T

import Graphics.Types
import OutputHandles.Types

fontWidth :: Graphics -> Double
fontWidth (Graphics _ (w, _)) = w

fontHeight :: Graphics -> Double
fontHeight (Graphics _ (_, h)) = h

wrapText :: Graphics -> T.Text -> Int -> Int -> Int -> Int -> Int -> Color -> ([TextDisplay], Int)
wrapText outs txt x yStart width lineSpace fontScale c = (wrapped, fromIntegral lastY)
    where
        (wrapped, lastY) = foldl makeTextDisplays ([], fromIntegral yStart) lines
        breakToWords = T.words txt
        (ls, last) = foldl makeLines ([], "") breakToWords
        lines = ls ++ [last]
        maxLetters = floor $ fromIntegral width / (fontWidth outs * fromIntegral fontScale)
        makeLines (ls, curr) w
            | T.length curr == 0 = (ls, w)
            | T.length curr + T.length w <= maxLetters = (ls, T.concat [curr, " ", w])
            | otherwise = (ls ++ [curr], w)
        yAdjust = lineSpace + ceiling (fontHeight outs * fromIntegral fontScale)
        makeTextDisplays (ts, y) line = (ts ++ [TextDisplay line (fromIntegral x) y fontScale c], y + fromIntegral yAdjust)


wrapTexts :: Graphics -> [T.Text] -> Int -> Int -> Int -> Int -> Int -> Color -> ([TextDisplay], Int)
wrapTexts outs txts x yStart width lineSpace fontScale c = foldl makeTextDisplays ([], yStart) txts
    where
        makeTextDisplays (ts, y) txt = let (newTs, newY) = wrapText outs txt x y width lineSpace fontScale c in (ts ++ newTs, newY)

adjustTexts :: Graphics -> [(TextDisplay, Int, Int)] -> ([TextDisplay], Int)
adjustTexts outs txts = foldl makeTextDisplays ([], 0) txts
    where
        makeTextDisplays (ts, y) (TextDisplay txt x _ fontScale c, width, lineSpace) =
            let (newTs, newY) = wrapText outs txt (fromIntegral x) y width lineSpace fontScale c
            in (ts ++ newTs, newY)

oneLine :: Graphics -> [(T.Text, Color, Int)] -> Int -> Int -> Int -> [TextDisplay]
oneLine outs txts x y space = fst $ foldl makeTextDisplays ([], fromIntegral x) txts
    where
        makeTextDisplays (ts, x') (txt, c, fontScale) =
            let letterWidth = fontWidth outs * fromIntegral fontScale
                lineLength = ceiling $ (fromIntegral (T.length txt)) * letterWidth
                spacing = floor $ fromIntegral space * letterWidth
            in (ts ++ [TextDisplay txt x' (fromIntegral y) fontScale c], x' + fromIntegral lineLength + fromIntegral spacing)
