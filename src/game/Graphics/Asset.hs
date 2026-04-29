module Graphics.Asset
    ( resizeGameView
    , staticAsset
    , staticText
    , backgroundAsset
    , centerTextX
    , centerTextY
    , centerText
    ) where

import qualified Data.Text as T

import Graphics.TextUtil
import Graphics.Types
import Graphics.NewTypes

import OutputHandles.Types

resizeGameView :: a -> Graphics -> GView a -> GView a
resizeGameView gps gr (GView ats ovs) = GView (doResize <$> ats) ((\(AOverlay o ia) -> AOverlay (doResize <$> o) ia) <$> ovs)
    where
        doResize asset = case assetResize asset of
                            Nothing -> asset
                            Just fn -> fn gps gr

staticAsset :: AssetObj -> Int -> Int -> Double -> Int -> Asset a
staticAsset o x y s l = Asset o x y s l True Nothing Nothing Nothing

staticText :: T.Text -> Color -> Int -> Int -> Double -> Int -> Asset a
staticText t c x y s l = Asset (AssetText t c) x y s l True Nothing Nothing Nothing

centerTextX :: Graphics -> T.Text -> Color -> Int -> Int -> Double -> Int -> Asset a
centerTextX gr txt c xOff y s l = asset $ xMid gr
    where
        asset x = Asset (AssetText txt c) (x + xOff) y s l True (Just resize) Nothing Nothing
        resize _ gr' = asset $ xMid gr'
        letterWidth gr' = fontWidth gr' * s
        textWidth gr' = fromIntegral (T.length txt) * letterWidth gr'
        xMid gr' = (graphicsWindowWidth gr' - (floor (textWidth gr'))) `div` 2

centerTextY :: Graphics -> T.Text -> Color -> Int -> Int -> Double -> Int -> Asset a
centerTextY gr txt c x yOff s l = asset $ yMid gr
    where
        asset y = Asset (AssetText txt c) x (y + yOff) s l True (Just resize) Nothing Nothing
        resize _ gr' = asset $ yMid gr'
        letterHeight gr' = fontHeight gr' * s
        yMid gr' = (graphicsWindowHeight gr' - (floor (letterHeight gr'))) `div` 2


centerText :: Graphics -> T.Text -> Color -> Int -> Int -> Double -> Int -> Asset a
centerText gr txt c xOff yOff s l = asset (midX gr) (midY gr)
    where
        asset x y = Asset (AssetText txt c) (x + xOff) (y + yOff) s l True (Just resize) Nothing Nothing
        resize _ gr' = asset (midX gr') (midY gr')
        letterWidth gr' = fontWidth gr' * s
        textWidth gr' = fromIntegral (T.length txt) * letterWidth gr'
        letterHeight gr' = fontHeight gr' * s
        midX gr' = (graphicsWindowWidth gr' - (floor (textWidth gr'))) `div` 2
        midY gr' = (graphicsWindowHeight gr' - (floor (letterHeight gr'))) `div` 2


backgroundAsset :: Graphics -> Color -> Asset as
backgroundAsset gr c = asset (AssetRect maxX maxY c)
    where
        asset rect = Asset rect 0 0 1 0 True (Just resize) Nothing Nothing
        maxX = graphicsWindowWidth gr
        maxY = graphicsWindowHeight gr
        resize _ gr = asset $ AssetRect (graphicsWindowWidth gr) (graphicsWindowHeight gr) c
