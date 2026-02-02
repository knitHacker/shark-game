-- test/RenderSpec.hs
module RenderSpec where

import Test.Hspec
import Test.QuickCheck

import Foreign.C.Types (CInt)
import qualified Data.Map.Strict as M

import OutputHandles.Types
import OutputHandles.Util
import OutputHandles.Text

data BoundsCheck = BoundsCheck
    { boundsOk :: Bool
    , outOfBounds :: [BoundsViolation]
    }

data BoundsViolation
    = OffLeft Draw Int
    | OffRight Draw Int
    | OffTop Draw Int
    | OffBottom Draw Int
    | NegativeWidth Draw
    | NegativeHeight Draw

checkBounds :: FontSize -> (Int, Int) -> ToRender -> BoundsCheck
checkBounds fs (screenW, screenH) render =
    let violations = concatMap (checkDraw fs screenW screenH) (renderDraws render)
    in BoundsCheck (null violations) violations

checkDraw :: FontSize -> Int -> Int -> Draw -> [BoundsViolation]
checkDraw fs w h d = catMaybes
    [ checkLeft d (getMinX d)
    , checkTop d (getMinY d)
    , checkRight d (getMaxX fs d) w
    , checkBottom d (getMaxY fs d) h
    , checkNegW d
    , checkNegH fs d
    ]
    where
        checkLeft d x = if x < 0 then Just (OffLeft d x) else Nothing
        checkTop d y = if y < 0 then Just (OffTop d y) else Nothing
        checkRight d x w = if x > w then Just (OffRight d (x - w)) else Nothing
        checkBottom d y h = if y > h then Just (OffBottom d (y - h)) else Nothing
        checkNegW d = if getWidth d < 0 then Just (NegativeWidth d) else Nothing
        checkNegH fs d = if getHeight fs d < 0 then Just (NegativeHeight d) else Nothing

getMinX :: Draw -> Int
getMinX (DrawTexture dt) = fromIntegral $ drawPosX dt
getMinX (DrawRectangle dr) = fromIntegral $ rectPosX dr
getMinX (DrawTextDisplay td) = fromIntegral $ wordsPosX td
getMinX (DrawAnimFrame daf) = fromIntegral $ drawPosAX daf

getMinY :: Draw -> Int
getMinY (DrawTexture dt) = fromIntegral $ drawPosY dt
getMinY (DrawRectangle dr) = fromIntegral $ rectPosY dr
getMinY (DrawTextDisplay td) = fromIntegral $ wordsPosY td
getMinY (DrawAnimFrame daf) = fromIntegral $ drawPosAY daf

getMaxX :: FontSize -> Draw -> Int
getMaxX _ (DrawTexture dt) = fromIntegral $ drawPosX dt + drawWidth dt
getMaxX _ (DrawRectangle dr) = fromIntegral $ rectPosX dr + rectWidth dr
getMaxX fs (DrawTextDisplay td) = fromIntegral (wordsPosX td) + ceiling (widthTextDisplay fs td)
getMaxX _ (DrawAnimFrame daf) = fromIntegral $ drawPosAX daf + drawAWidth daf

getMaxY :: FontSize -> Draw -> Int
getMaxY _ (DrawTexture dt) = fromIntegral $ drawPosY dt + drawHeight dt
getMaxY _ (DrawRectangle dr) = fromIntegral $ rectPosY dr + rectHeight dr
getMaxY fs (DrawTextDisplay td) = fromIntegral (wordsPosY td) + ceiling (heightTextDisplay fs td)
getMaxY _ (DrawAnimFrame daf) = fromIntegral $ drawPosAY daf + drawAHeight daf

getWidth :: Draw -> Int
getWidth (DrawTexture dt) = fromIntegral $ drawWidth dt
getWidth (DrawRectangle dr) = fromIntegral $ rectWidth dr
getWidth (DrawTextDisplay td) = T.length (wordsText td) -- approximation
getWidth (DrawAnimFrame daf) = fromIntegral $ drawAWidth daf

getHeight :: FontSize -> Draw -> Int
getHeight _ (DrawTexture dt) = fromIntegral $ drawHeight dt
getHeight _ (DrawRectangle dr) = fromIntegral $ rectHeight dr
getHeight fs (DrawTextDisplay td) = ceiling $ heightTextDisplay fs td
getHeight _ (DrawAnimFrame daf) = fromIntegral $ drawAHeight daf

-- Overlap detection
data Overlap = Overlap Draw Draw

findOverlaps :: FontSize -> ToRender -> [Overlap]
findOverlaps fs render = go (renderDraws render)
    where
        go [] = []
        go (d:ds) = mapMaybe (checkOverlap fs d) ds ++ go ds

checkOverlap :: FontSize -> Draw -> Draw -> Maybe Overlap
checkOverlap fs a b
    | rectanglesIntersect (toBounds fs a) (toBounds fs b) = Just (Overlap a b)
    | otherwise = Nothing

toBounds :: FontSize -> Draw -> (Int, Int, Int, Int)
toBounds fs d = (getMinX d, getMinY d, getMaxX fs d, getMaxY fs d)

rectanglesIntersect :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
rectanglesIntersect (ax1, ay1, ax2, ay2) (bx1, by1, bx2, by2) =
    ax1 < bx2 && ax2 > bx1 && ay1 < by2 && ay2 > by1
