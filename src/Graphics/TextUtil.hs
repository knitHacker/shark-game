{-# LANGUAGE OverloadedStrings #-}

module Graphics.TextUtil
    ( fontWidth
    , fontHeight
    , wrapText
    , wrapTexts
    , adjustTexts
    , oneLine
    , rightJustSpacing
    , splitJustSpacing
    , showMoney
    , textMiddleX
    , wrapTextMiddleX
    , midStart
    , percentWidth
    ) where

import qualified Data.Text as T

import Graphics.Types
import OutputHandles.Types
import Data.Text (Text)

percentWidth :: Graphics -> Double -> Int
percentWidth gr perc = floor (fromIntegral (graphicsWindowWidth gr) * perc)

midStart :: Graphics -> Int -> Int
midStart gr partLen = (graphicsWindowWidth gr - partLen) `div` 2

fontWidth :: Graphics -> Double
fontWidth gr = fst $ graphicsFontSize gr

fontHeight :: Graphics -> Double
fontHeight gr = snd $ graphicsFontSize gr

textMiddleX :: Graphics -> T.Text -> Int -> Int -> Color -> (TextDisplay, Int)
textMiddleX gr txt yPos fontScale c = (TextDisplay txt xMid (fromIntegral yPos) fontScale c Nothing, fromIntegral xMid)
    where
        letterWidth = fontWidth gr * fromIntegral fontScale
        textWidth = fromIntegral (T.length txt) * letterWidth
        xMid = fromIntegral $ midStart gr (floor textWidth)

wrapTextMiddleX :: Graphics -> T.Text -> Int -> Int -> Int -> Int -> Color -> ([TextDisplay], Int)
wrapTextMiddleX gr txt margin yStart lineSpace fontScale c =
    wrapText gr txt margin yStart width lineSpace fontScale c
    where
        width = graphicsWindowWidth gr - 2 * margin
        letterWidth = fontWidth gr * fromIntegral fontScale

wrapText :: Graphics -> T.Text -> Int -> Int -> Int -> Int -> Int -> Color -> ([TextDisplay], Int)
wrapText gr txt x yStart width lineSpace fontScale c = (wrapped, fromIntegral lastY)
    where
        (wrapped, lastY) = foldl makeTextDisplays ([], fromIntegral yStart) lines
        breakToWords = T.words txt
        (ls, last) = foldl makeLines ([], "") breakToWords
        lines = ls ++ [last]
        maxLetters = floor $ fromIntegral width / (fontWidth gr * fromIntegral fontScale)
        makeLines (ls, curr) w
            | T.length curr == 0 = (ls, w)
            | T.length curr + T.length w <= maxLetters = (ls, T.concat [curr, " ", w])
            | otherwise = (ls ++ [curr], w)
        yAdjust = lineSpace + ceiling (fontHeight gr * fromIntegral fontScale)
        makeTextDisplays (ts, y) line = (ts ++ [TextDisplay line (fromIntegral x) y fontScale c Nothing], y + fromIntegral yAdjust)


wrapTexts :: Graphics -> [T.Text] -> Int -> Int -> Int -> Int -> Int -> Color -> ([TextDisplay], Int)
wrapTexts outs txts x yStart width lineSpace fontScale c = foldl makeTextDisplays ([], yStart) txts
    where
        makeTextDisplays (ts, y) txt = let (newTs, newY) = wrapText outs txt x y width lineSpace fontScale c in (ts ++ newTs, newY)

adjustTexts :: Graphics -> [(TextDisplay, Int, Int)] -> ([TextDisplay], Int)
adjustTexts outs = foldl makeTextDisplays ([], 0)
    where
        makeTextDisplays (ts, y) (TextDisplay txt x _ fontScale c Nothing, width, lineSpace) =
            let (newTs, newY) = wrapText outs txt (fromIntegral x) y width lineSpace fontScale c
            in (ts ++ newTs, newY)

oneLine :: Graphics -> [(T.Text, Color, Int)] -> Int -> Int -> Int -> [TextDisplay]
oneLine outs txts x y space = fst $ foldl makeTextDisplays ([], fromIntegral x) txts
    where
        makeTextDisplays (ts, x') (txt, c, fontScale) =
            let letterWidth = fontWidth outs * fromIntegral fontScale
                lineLength = ceiling $ fromIntegral (T.length txt) * letterWidth
                spacing = floor $ fromIntegral space * letterWidth
            in (ts ++ [TextDisplay txt x' (fromIntegral y) fontScale c Nothing], x' + fromIntegral lineLength + fromIntegral spacing)


rightJustSpacing :: [T.Text] -> [T.Text]
rightJustSpacing txts = zipWith addSpaces txts diffs
    where
        lengths = T.length <$> txts
        maxLen = maximum lengths
        diffs = (maxLen -) <$> lengths
        addSpaces txt diff = T.append (T.replicate diff " ") txt

splitJustSpacing :: [(T.Text, T.Text)] -> [T.Text]
splitJustSpacing txts = zipWith addSpaces txts diffs
    where
        leftLengths = T.length <$> fst <$> txts
        rightLengths = T.length <$> snd <$> txts
        maxLeftLen = maximum leftLengths
        maxRightLen = maximum rightLengths
        diffs = zipWith (\l r -> maxLeftLen - l + maxRightLen - r) leftLengths rightLengths
        addSpaces (txtL, txtR) diff = T.concat [txtL, T.replicate diff " ", txtR]


showMoney :: Int -> T.Text
showMoney m = T.pack $ "$" ++ insertCommas nStr
    where
        nStr = show m
        insertCommas ns = reverse $ insertCommas' $ reverse ns
        insertCommas' ns
            | length ns <= 3 = ns
            | otherwise = take 3 ns ++ "," ++ insertCommas' (drop 3 ns)