{-# LANGUAGE OverloadedStrings #-}

module OutputHandles.Util
    ( wrapText
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
