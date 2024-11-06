{-# LANGUAGE OverloadedStrings #-}

module OutputHandles.Util
    ( wrapText
    , wrapTexts
    , adjustTexts
    , oneLine
    ) where

import Foreign.C.Types ( CInt )
import qualified Data.Text as T

import OutputHandles.Types

wrapText :: OutputHandles -> T.Text -> CInt -> CInt -> CInt -> CInt -> Int -> Color -> ([TextDisplay], CInt)
wrapText outs txt x yStart width lineSpace fontScale c = foldl makeTextDisplays ([], yStart) lines
    where
        breakToWords = T.words txt
        (ls, last) = foldl makeLines ([], "") breakToWords
        lines = ls ++ [last]
        maxLetters = floor $ fromIntegral width / (fontWidth outs * fromIntegral fontScale)
        makeLines (ls, curr) w
            | T.length curr == 0 = (ls, w)
            | T.length curr + T.length w <= maxLetters = (ls, T.concat [curr, " ", w])
            | otherwise = (ls ++ [curr], w)
        yAdjust = lineSpace + floor (fontHeight outs * fromIntegral fontScale)
        makeTextDisplays (ts, y) line = (ts ++ [TextDisplay line x y fontScale c], y + yAdjust)


wrapTexts :: OutputHandles -> [T.Text] -> CInt -> CInt -> CInt -> CInt -> Int -> Color -> ([TextDisplay], CInt)
wrapTexts outs txts x yStart width lineSpace fontScale c = foldl makeTextDisplays ([], yStart) txts
    where
        makeTextDisplays (ts, y) txt = let (newTs, newY) = wrapText outs txt x y width lineSpace fontScale c in (ts ++ newTs, newY)

adjustTexts :: OutputHandles -> [(TextDisplay, CInt, CInt)] -> ([TextDisplay], CInt)
adjustTexts outs txts = foldl makeTextDisplays ([], 0) txts
    where
        makeTextDisplays (ts, y) (TextDisplay txt x _ fontScale c, width, lineSpace) =
            let (newTs, newY) = wrapText outs txt x y width lineSpace fontScale c
            in (ts ++ newTs, newY)

oneLine :: OutputHandles -> [(T.Text, Color, Int)] -> CInt -> CInt -> CInt -> [TextDisplay]
oneLine outs txts x y space = fst $ foldl makeTextDisplays ([], x) txts
    where
        makeTextDisplays (ts, x') (txt, c, fontScale) =
            let letterWidth = floor $ (fontWidth outs * fromIntegral fontScale)
                lineLength = fromIntegral (T.length txt) * letterWidth
                spacing = space * letterWidth
            in (ts ++ [TextDisplay txt x' y fontScale c], x' + lineLength + spacing)
