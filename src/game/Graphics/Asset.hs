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
    , assetObjWidth
    , assetObjHeight
    ) where

import qualified Data.Text as T

import qualified Data.Map.Strict as M

import Graphics.TextUtil
import Graphics.Types
import Graphics.NewTypes

import OutputHandles.Types

assetObjWidth :: Graphics -> AssetObj -> Int
assetObjWidth gr (AssetText txt _ sz) = floor $ fromIntegral (T.length txt) * fontWidth gr * fromIntegral sz
assetObjWidth _  (AssetRect w _ _) = w
assetObjWidth gr (AssetImage img scale) = maybe 0 (\i -> floor $ fromIntegral (imageSizeX i) * scale) (M.lookup img (graphicsStaticTextures gr))
assetObjWidth gr (AssetAnimation img _ _ scale) = maybe 0 (\i -> floor $ fromIntegral (animSizeX i) * scale) (M.lookup img (graphicsAnimTextures gr))
assetObjWidth gr (AssetStacked items _) = maximum $ assetObjWidth gr . stackItem <$> items
assetObjWidth gr (AssetMenu m) = maximum $ (\i -> floor $ fromIntegral (T.length (menuItemText i)) * fontWidth gr * fromIntegral (menuItemFontSize i)) <$> menuItems m
assetObjWidth gr (AssetScroll sc) = maximum $ (\t -> floor $ fromIntegral (T.length t) * fontWidth gr * fromIntegral (scrollTextSize sc)) <$> scrollText sc


assetObjHeight :: Graphics -> AssetObj -> Int
assetObjHeight gr (AssetText _ _ sz) = ceiling $ fontHeight gr * fromIntegral sz
assetObjHeight _  (AssetRect _ h _) = h
assetObjHeight gr (AssetImage img scale) = maybe 0 (\i -> floor $ fromIntegral (imageSizeY i) * scale) (M.lookup img (graphicsStaticTextures gr))
assetObjHeight gr (AssetAnimation img _ _ scale) = maybe 0 (\i -> floor $ fromIntegral (animSizeY i) * scale) (M.lookup img (graphicsAnimTextures gr))
assetObjHeight gr (AssetStacked items sp) = sum $ (\i -> assetObjHeight gr (stackItem i) + sp) <$> items
assetObjHeight gr (AssetMenu m) = let itemH i = ceiling (fontHeight gr * fromIntegral (menuItemFontSize i)) + menuLineSpace m
                                  in sum $ itemH <$> menuItems m
assetObjHeight gr (AssetScroll sc) = let lh = ceiling (fontHeight gr * fromIntegral (scrollTextSize sc)) + scrollLineSpace sc
                                     in length (scrollText sc) * lh




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


wrapTextAsset :: Graphics -> Int -> Color -> T.Text -> Int -> Int -> Int -> Int -> Bool -> Asset
wrapTextAsset gr percentWide c fullTxt sz sp l startY centerLine = asset gr
    where
        maxChar gr'   = floor $ fromIntegral (graphicsWindowWidth gr') * (fromIntegral percentWide / 100.0) / (fontWidth gr' * fromIntegral sz)
        lines gr'     = makeLines fullTxt (maxChar gr')
        stackW gr'    = floor $ fromIntegral (graphicsWindowWidth gr') * (fromIntegral percentWide / 100.0)
        lineXOff gr' txt
            | centerLine = (stackW gr' - floor (fromIntegral (T.length txt) * fontWidth gr' * fromIntegral sz)) `div` 2
            | otherwise  = 0
        stack gr'     = (\txt -> StackItem (AssetText txt c sz) (lineXOff gr' txt)) <$> lines gr'
        xStart gr'    = floor $ fromIntegral (graphicsWindowWidth gr') * ((100.0 - fromIntegral percentWide) / 100.0 / 2.0)
        asset gr'     = Asset (AssetStacked (stack gr') sp) (xStart gr') startY l True $ Just resize
        resize _ gr'  = asset gr'


appendAssetStackCenterX :: Graphics -> Asset -> AssetObj -> Asset
appendAssetStackCenterX gr baseAsset newObj = appendItem gr baseAsset
    where
        xOff gr' parentX = midStartX gr' (assetObjWidth gr' newObj) - parentX
        appendItem gr' a =
            let stack' = case object a of
                    AssetStacked stack sp -> AssetStacked (stack ++ [StackItem newObj (xOff gr' (assetX a))]) sp
                    obj                   -> AssetStacked [StackItem obj 0, StackItem newObj (xOff gr' (assetX a))] 2
            in a { object = stack', assetResize = newResize }
        newResize = fmap (\fn _ gr' -> appendItem gr' (fn baseAsset gr')) (assetResize baseAsset)
