module OutputHandles.Text
    ( heightTextDisplay
    , widthTextDisplay
    , addText
    , getTextMaxX
    , getTextMaxY
    , getTextMinX
    , getTextMinY
    ) where

import qualified Data.Text as T

import OutputHandles.Types


heightTextDisplay :: FontSize -> TextDisplay -> Double
heightTextDisplay (_, fh) td = fromIntegral (wordsSize td) * fh

widthTextDisplay :: FontSize -> TextDisplay -> Double
widthTextDisplay (fw, _) td = fromIntegral (wordsSize td * (T.length (wordsText td))) * fw

getTextMinY :: [TextDisplay] -> Maybe Int
getTextMinY ts = if null ts then Nothing else Just (minimum $ map (fromIntegral . wordsPosY) ts)

getTextMinX :: [TextDisplay] -> Maybe Int
getTextMinX ts = if null ts then Nothing else Just (minimum $ map (fromIntegral . wordsPosX) ts)

getTextMaxY :: FontSize -> [TextDisplay] -> Maybe Int
getTextMaxY fs ts = if null ts then Nothing else Just $ ceiling (maximum $ map (\t -> fromIntegral (wordsPosY t) + heightTextDisplay fs t) ts)

getTextMaxX :: FontSize -> [TextDisplay] -> Maybe Int
getTextMaxX fs ts = if null ts then Nothing else Just $ ceiling (maximum $ map (\t -> fromIntegral (wordsPosX t) + widthTextDisplay fs t) ts)

addText :: ToRender -> Int -> Int -> TextDisplay -> ToRender
addText rend depth priority td = addDraw rend depth priority (DrawTextDisplay td)
