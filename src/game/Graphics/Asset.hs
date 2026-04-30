{-# LANGUAGE OverloadedStrings #-}

module Graphics.Asset
    ( resizeGameView
    , staticAsset
    , staticText
    , backgroundAsset
    , centerTextX
    , centerTextY
    , centerText
    , wrapTextAsset
    , appendAssetStackCenterX
    ) where

import qualified Data.Text as T

import Graphics.TextUtil
import Graphics.Types
import Graphics.NewTypes

import OutputHandles.Types

resizeGameView :: a -> Graphics -> GView -> GView
resizeGameView gps gr (GView ats ovs) = GView (doResize <$> ats) ((\(AOverlay o ia) -> AOverlay (doResize <$> o) ia) <$> ovs)
    where
        doResize asset = case assetResize asset of
                            Nothing -> asset
                            Just fn -> fn asset gr

staticAsset :: AssetObj -> Int -> Int -> Int -> Asset
staticAsset o x y l = Asset o x y l True Nothing

staticText :: T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
staticText t c x y s l = Asset (AssetText t c s) x y l True Nothing

centerTextX :: Graphics -> T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
centerTextX gr txt c xOff y s l = asset $ xMid gr
    where
        asset x = Asset (AssetText txt c s) (x + xOff) y l True (Just resize)
        resize asset' gr' = asset' { assetX = xMid gr' }
        letterWidth gr' = fontWidth gr' * fromIntegral s
        textWidth gr' = fromIntegral (T.length txt) * letterWidth gr'
        xMid gr' = (graphicsWindowWidth gr' - (floor (textWidth gr'))) `div` 2

centerTextY :: Graphics -> T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
centerTextY gr txt c x yOff s l = asset $ yMid gr
    where
        asset y = Asset (AssetText txt c s) x (y + yOff) l True (Just resize)
        resize asset' gr' = asset' { assetY = yMid gr' }
        letterHeight gr' = fontHeight gr' * fromIntegral s
        yMid gr' = (graphicsWindowHeight gr' - (floor (letterHeight gr'))) `div` 2


centerText :: Graphics -> T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
centerText gr txt c xOff yOff s l = asset (midX gr) (midY gr)
    where
        asset x y = Asset (AssetText txt c s) (x + xOff) (y + yOff) l True (Just resize)
        resize asset' gr' = asset' { assetX = (midX gr') + xOff, assetY = (midY gr') + yOff }
        letterWidth gr' = fontWidth gr' * fromIntegral s
        textWidth gr' = fromIntegral (T.length txt) * letterWidth gr'
        letterHeight gr' = fontHeight gr' * fromIntegral s
        midX gr' = (graphicsWindowWidth gr' - (floor (textWidth gr'))) `div` 2
        midY gr' = (graphicsWindowHeight gr' - (floor (letterHeight gr'))) `div` 2


backgroundAsset :: Graphics -> Color -> Asset
backgroundAsset gr c = asset (AssetRect maxX maxY c)
    where
        asset rect = Asset rect 0 0 0 True (Just resize)
        maxX = graphicsWindowWidth gr
        maxY = graphicsWindowHeight gr
        resize asset' gr = asset' { object = AssetRect (graphicsWindowWidth gr) (graphicsWindowHeight gr) c }

makeLines :: T.Text -> Int -> [T.Text]
makeLines txt maxChar = ls ++ [last]
    where
        words = T.words txt
        (ls, last) = foldl makeLines ([], "") words
        makeLines (ls, curr) w
            | T.length curr == 0 = (ls, w)
            | T.length curr + T.length w <= maxChar = (ls, T.concat [curr, " ", w])
            | otherwise = (ls ++ [curr], w)

wrapTextAsset :: Graphics -> Int -> Color -> T.Text -> Int -> Int -> Int -> Int -> Asset
wrapTextAsset gr percentWide c fullTxt sz sp l startY = asset gr
    where
        maxChar gr' = floor $ (fromIntegral (graphicsWindowWidth gr')) * ((fromIntegral percentWide) / 100.0) / (fontWidth gr' * fromIntegral sz)
        lines gr' = makeLines fullTxt (maxChar gr')
        stack gr' = (\txt -> StackItem (AssetText txt c sz) 0) <$> lines gr'
        xStart gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (((100.0 - (fromIntegral percentWide)) / 100.0) / 2.0)
        asset gr' = Asset (AssetStacked (stack gr') sp) (xStart gr') startY l True $ Just resize
        resize _ gr' = asset gr'

appendAssetStackCenterX :: Asset -> AssetObj -> Asset
appendAssetStackCenterX asset newObj =
    case object asset of
        (AssetStacked stack sp) ->
            let newStack = AssetStacked (stack ++ [StackItem newObj 0]) sp
                --resize' ass' gr' =
            in asset { object = newStack } -- todo: fix resize
        obj -> asset { object = AssetStacked [StackItem obj 0, StackItem newObj 0] 2}