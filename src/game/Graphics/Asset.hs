{-# LANGUAGE OverloadedStrings #-}

module Graphics.Asset
    ( resizeGameView
    , staticAsset
    , staticText
    , backgroundAsset
    , centerAssetX
    , centerTextX
    , centerTextY
    , centerText
    , wrapTextAsset
    , appendAssetStackCenterX
    , appendAssetStackWith
    , appendAssetStack
    , assetObjWidth
    , assetObjHeight
    , getCenterX
    , mkResizeAsset
    , nextMenu
    , singleMenu
    , changeHighlight
    , changeCursor
    , applyAnimationState
    , oneLineText
    , resizeAssets
    , resizeMenu
    ) where

import qualified Data.Text as T

import qualified Data.Map.Strict as M
import Data.Int (Int64)

import Graphics.TextUtil
import Graphics.Types
import Graphics.NewTypes

import OutputHandles.Types

applyAnimationState :: Int64 -> Graphics -> GView -> AnimationState -> GView
applyAnimationState ts gr gv (AnimState aTs _ assetIds update)
    | ts == aTs = gv { assets = assets' }
    | otherwise = gv
    where
        assets' = foldl fn (assets gv) assetIds
        fn assetMap id = M.adjust (\a -> update a gr) id assetMap

changeHighlight :: MenuAsset -> Int -> Color -> MenuAsset
changeHighlight ma idx c = ma { menuItems = updateItems <$> zip [0..] (menuItems ma) }
    where
        updateItems (i, mi)
            | i == idx = mi { highlightedColor = Just c }
            | otherwise = mi { highlightedColor = Nothing }


changeCursor :: MenuAsset -> Int -> Image -> Double -> MenuAsset
changeCursor ma idx img s = ma { menuItems = updateItems <$> zip [0..] (menuItems ma) }
    where
        updateItems (i, mi)
            | i == idx = mi { cursorImg = Just (img, s) }
            | otherwise = mi { cursorImg = Nothing }


assetObjWidth :: Graphics -> AssetObj -> Int
assetObjWidth gr (AssetText txt _ sz) = floor $ fromIntegral (T.length txt) * fontWidth gr * fromIntegral sz
assetObjWidth _  (AssetRect w _ _) = w
assetObjWidth gr (AssetImage img scale) = maybe 0 (\i -> floor $ fromIntegral (imageSizeX i) * scale) (M.lookup img (graphicsStaticTextures gr))
assetObjWidth gr (AssetAnimation img _ _ scale) = maybe 0 (\i -> floor $ fromIntegral (animSizeX i) * scale) (M.lookup img (graphicsAnimTextures gr))
assetObjWidth gr (AssetStacked StackVertical items _)   = maximum $ assetObjWidth gr . stackItem <$> items
assetObjWidth gr (AssetStacked StackHorizontal items sp) = sum $ (\i -> assetObjWidth gr (stackItem i) + sp) <$> items
assetObjWidth gr (AssetScroll sc) = maximum $ (\t -> floor $ fromIntegral (T.length t) * fontWidth gr * fromIntegral (scrollTextSize sc)) <$> scrollText sc


assetObjHeight :: Graphics -> AssetObj -> Int
assetObjHeight gr (AssetText _ _ sz) = ceiling $ fontHeight gr * fromIntegral sz
assetObjHeight _  (AssetRect _ h _) = h
assetObjHeight gr (AssetImage img scale) = maybe 0 (\i -> floor $ fromIntegral (imageSizeY i) * scale) (M.lookup img (graphicsStaticTextures gr))
assetObjHeight gr (AssetAnimation img _ _ scale) = maybe 0 (\i -> floor $ fromIntegral (animSizeY i) * scale) (M.lookup img (graphicsAnimTextures gr))
assetObjHeight gr (AssetStacked StackVertical items sp)   = sum $ (\i -> assetObjHeight gr (stackItem i) + sp) <$> items
assetObjHeight gr (AssetStacked StackHorizontal items _) = maximum $ (\i -> stackYOff i + assetObjHeight gr (stackItem i)) <$> items
assetObjHeight gr (AssetScroll sc) = let lh = ceiling (fontHeight gr * fromIntegral (scrollTextSize sc)) + scrollLineSpace sc
                                     in length (scrollText sc) * lh


resizeGameView :: a -> Graphics -> GView -> GView
resizeGameView gps gr gv@(GView ats ovs _ menuAsset) = gv { assets = resizeAssets ats gr, overlays = doOResize <$> ovs, menuAsset = resizeMenu menuAsset gr}
    where
        doOResize over = maybe over (\rsFn -> rsFn over gr) (resizeOverlay over)

resizeMenu :: Maybe MenuAsset -> Graphics -> Maybe MenuAsset
resizeMenu Nothing _ = Nothing
resizeMenu (Just menu) gr = Just $ maybe menu (\rFn -> rFn menu gr) (menuResize menu)


resizeAssets :: M.Map AssetId Asset -> Graphics -> M.Map AssetId Asset
resizeAssets assets gr = doResize <$> assets
    where
        doResize asset = case assetResize asset of
                            Nothing -> asset
                            Just fn -> fn asset gr


staticAsset :: AssetObj -> Int -> Int -> Int -> Asset
staticAsset o x y l = Asset o x y l True Nothing

staticText :: T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
staticText t c x y s l = Asset (AssetText t c s) x y l True Nothing

oneLineText :: [(T.Text, Color, Int)] -> Int -> Int -> Int -> Int -> Asset
oneLineText tInfo sp x y l = Asset stack x y l True Nothing
    where
        stack = AssetStacked StackHorizontal (mkTxtAsset <$> tInfo) sp
        mkTxtAsset (txt, c, sz) = StackItem (AssetText txt c sz) 0 0

-- assumes resize overwrites x and y
mkResizeAsset :: Graphics -> AssetObj -> Int -> Bool -> Resize -> Asset
mkResizeAsset gr obj l vis rFn = rFn (Asset obj undefined undefined l vis (Just rFn)) gr


singleMenu :: Graphics -> T.Text -> Int -> MenuAsset
singleMenu gr txt l = MenuAsset (txtX gr) (graphicsWindowHeight gr - 100) l (Just menuResize) False 0 [MenuItem txt Blue sz 0 0 (Just White) Nothing]
    where
        sz = 3
        txtX gr' = midTextStart gr' txt (fromIntegral sz)
        menuResize asset' gr' = asset' { menuXBase = txtX gr', menuYBase = graphicsWindowHeight gr' - 100 }

nextMenu :: Graphics -> Int -> MenuAsset
nextMenu gr l = singleMenu gr "Next" l

centerAssetX :: Graphics -> AssetObj -> Int -> Int -> Int -> Asset
centerAssetX gr obj xOff y l = mkResizeAsset gr obj l True resize
    where
        resize asset' gr' = asset' { assetX = xMid gr' + xOff, assetY = y }
        xMid gr' = (graphicsWindowWidth gr' - assetObjWidth gr' obj) `div` 2


centerTextX :: Graphics -> T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
centerTextX gr txt c s xOff y l = centerAssetX gr (AssetText txt c s) xOff y l


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
        maxChar gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (fromIntegral percentWide / 100.0) / (fontWidth gr' * fromIntegral sz)
        lines gr' = makeLines fullTxt (maxChar gr')
        stackW gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (fromIntegral percentWide / 100.0)
        lineXOff gr' txt
            | centerLine = (stackW gr' - floor (fromIntegral (T.length txt) * fontWidth gr' * fromIntegral sz)) `div` 2
            | otherwise  = 0
        stack gr' = (\txt -> StackItem (AssetText txt c sz) (lineXOff gr' txt) 0) <$> lines gr'
        xStart gr' = floor $ fromIntegral (graphicsWindowWidth gr') * ((100.0 - fromIntegral percentWide) / 100.0 / 2.0)
        asset gr' = Asset (AssetStacked StackVertical (stack gr') sp) (xStart gr') startY l True $ Just resize
        resize _ gr' = asset gr'


appendAssetStackCenterX :: Graphics -> StackDir -> Asset -> Int -> Int -> AssetObj -> Asset
appendAssetStackCenterX gr dir baseAsset xOff yOff newObj = appendItem gr baseAsset
    where
        xOffFn gr' parentX = midStartX gr' (assetObjWidth gr' newObj) - parentX + xOff
        appendItem gr' a =
            let stack' = case object a of
                    AssetStacked dir' stack sp
                        | dir' == dir -> AssetStacked dir' (stack ++ [StackItem newObj (xOffFn gr' (assetX a)) yOff]) sp
                    obj -> AssetStacked dir [StackItem obj 0 0, StackItem newObj (xOffFn gr' (assetX a)) yOff] 2
            in a { object = stack', assetResize = newResize }
        newResize = fmap (\fn _ gr' -> appendItem gr' (fn baseAsset gr')) (assetResize baseAsset)


appendAssetStack :: Graphics -> StackDir -> Asset -> Int -> Int -> AssetObj -> Asset
appendAssetStack gr dir baseAsset x yOff newObj = appendItem baseAsset
    where
        appendItem a =
            let xOff = x - assetX a
                stack' = case object a of
                    obj@(AssetStacked d stack sp)
                        | d == dir  -> AssetStacked d (stack ++ [StackItem newObj xOff yOff]) sp
                        | otherwise -> AssetStacked dir [StackItem obj 0 0, StackItem newObj xOff yOff] sp
                    obj -> case dir of
                            StackHorizontal -> AssetStacked dir [StackItem obj 0 0, StackItem newObj 0 yOff] xOff
                            StackVertical -> AssetStacked dir [StackItem obj 0 0, StackItem newObj xOff 0] yOff
            in a { object = stack', assetResize = combResize }
        combResize = fmap (\fn _ gr' -> appendItem (fn baseAsset gr')) (assetResize baseAsset)


appendAssetStackWith :: Graphics -> StackDir -> Asset -> (Int -> Int -> Graphics -> AssetObj) -> Asset
appendAssetStackWith gr dir baseAsset newObjFn = appendItem gr baseAsset
    where
        appendItem gr' a =
            let newObj = newObjFn (assetX a) (assetY a) gr'
                stack' = case object a of
                    AssetStacked d stack sp
                        | d == dir  -> AssetStacked d (stack ++ [StackItem newObj 0 0]) sp
                    obj -> AssetStacked dir [StackItem obj 0 0, StackItem newObj 0 0] 2
            in a { object = stack', assetResize = combResize }
        combResize = Just $ combResize' (assetResize baseAsset)
        combResize' (Just fn) _ gr' = appendItem gr' $ fn baseAsset gr'
        combResize' _ _ gr' = appendItem gr' baseAsset

getCenterX :: Graphics -> Int -> Int -> Int
getCenterX gr startX width = midStartX gr width - startX
    where
        windowW = graphicsWindowWidth gr
