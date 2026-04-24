{-# LANGUAGE OverloadedStrings #-}

module Graphics.TextUtil
    ( fontWidth
    , fontHeight
    , textWidth
    , rightJustSpacing
    , splitJustSpacing
    , showMoney
    , textMiddleX
    , midStartX
    , midStartY
    , midTextStart
    , percentWidth
    ) where

import qualified Data.Text as T
import qualified Data.List as L

import Graphics.Types
import OutputHandles.Types
import Data.Text (Text)


percentWidth :: Graphics -> Double -> Int
percentWidth gr perc = floor (fromIntegral (graphicsWindowWidth gr) * perc)

midStartX :: Graphics -> Int -> Int
midStartX gr partLen = (graphicsWindowWidth gr - partLen) `div` 2

midStartY :: Graphics -> Int -> Int
midStartY gr partLen = (graphicsWindowHeight gr - partLen) `div` 2

midTextStart :: Graphics -> T.Text -> Double -> Int
midTextStart gr txt fontScale = midStartX gr textWidth
    where
        letterWidth = fontWidth gr * fontScale
        textWidth = ceiling $ fromIntegral (T.length txt) * letterWidth

fontWidth :: Graphics -> Double
fontWidth gr = fst $ graphicsFontSize gr

fontHeight :: Graphics -> Double
fontHeight gr = snd $ graphicsFontSize gr

textWidth :: Graphics -> T.Text -> Int -> Int
textWidth gr txt fs = round $ fromIntegral (T.length txt) * fontWidth gr * fromIntegral fs

textMiddleX :: Graphics -> T.Text -> Int -> Int -> Color -> (TextDisplay, Int)
textMiddleX gr txt yPos fontScale c = (TextDisplay txt xMid (fromIntegral yPos) fontScale c Nothing, fromIntegral xMid)
    where
        letterWidth = fontWidth gr * fromIntegral fontScale
        textWidth = fromIntegral (T.length txt) * letterWidth
        xMid = fromIntegral $ midStartX gr (floor textWidth)

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
