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


heightTextDisplay :: FontSize -> TextDisplay -> Int
heightTextDisplay (_, fh) td = fromIntegral (wordsSize td) * fh

widthTextDisplay :: FontSize -> TextDisplay -> Int
widthTextDisplay (fw, _) td = fromIntegral (T.length (wordsText td)) * fw

getTextMinY :: [TextDisplay] -> Int
getTextMinY ts = minimum $ map (fromIntegral . wordsPosY) ts

getTextMinX :: [TextDisplay] -> Int
getTextMinX ts = minimum $ map (fromIntegral . wordsPosX) ts

getTextMaxY :: FontSize -> [TextDisplay] -> Int
getTextMaxY fs ts = maximum $ map (\t -> fromIntegral (wordsPosY t) + heightTextDisplay fs t) ts

getTextMaxX :: FontSize -> [TextDisplay] -> Int
getTextMaxX fs ts = maximum $ map (\t -> fromIntegral (wordsPosX t) + widthTextDisplay fs t) ts

addText :: ToRender -> Int -> Int -> TextDisplay -> ToRender
addText rend depth priority td = addDraw rend depth priority (DrawTextDisplay td)
