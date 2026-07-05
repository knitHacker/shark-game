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
    , changeScrollHighlight
    , changeCursor
    , applyAnimationState
    , oneLineText
    , oneLineTextRs
    , resizeAssets
    , resizeMenu
    , wrapTextStack
    , updateSomeMenuItems
    , updateMenuItems
    , centerScaleImgX
    , getScrollFullHeight
    , getMaxPosition
    , updateScrollPosition
    , updateOverlayHighlight
    , updateMinSet
    , makeTableMenu
    , scrollMenuResize
    , mkScrollMenu
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Int (Int64)
import Data.Maybe (isJust)

import Graphics.TextUtil
import Graphics.Types
import OutputHandles.Types

import Debug.Trace

updateMinSet :: Ord a => (a -> a) -> S.Set a -> S.Set a
updateMinSet up set
    | S.null set = set
    | otherwise =
        let (min, set') = S.deleteFindMin set
        in S.insert (up min) set'

updateOverlayHighlight :: Int -> Color -> GView -> GView
updateOverlayHighlight menuIdx c gv = gv { overlays = updateMinSet updateOv (overlays gv) }
    where
        updateOv ov = ov { oMenu = update $ oMenu ov }
        update (Just (DefaultMenu m)) = Just $ DefaultMenu $ changeHighlight m menuIdx c
        update mM = mM

applyAnimationState :: Int64 -> Graphics -> GView -> AnimationState -> GView
applyAnimationState ts gr gv (AnimState aTs _ assetIds update)
    | ts == aTs = gv { assets = assets' }
    | otherwise = gv
    where
        assets' = foldl fn (assets gv) assetIds
        fn assetMap id = M.adjust (\a -> update a gr) id assetMap

updateMenuItems :: MenuAsset -> (Int -> AssetMenuItem -> AssetMenuItem) -> MenuAsset
updateMenuItems ma upFn = ma { menuItems = (\(i, mi) -> upFn i mi) <$> zip [0..] (menuItems ma) }

updateSomeMenuItems :: MenuAsset -> (Int -> Bool) -> (AssetMenuItem -> AssetMenuItem) -> MenuAsset
updateSomeMenuItems ma shouldUpdate upF = ma { menuItems = (\(i, mi) -> if shouldUpdate i then upF mi else mi) <$> zip [0..] (menuItems ma) }

changeHighlight :: MenuAsset -> Int -> Color -> MenuAsset
changeHighlight ma idx c = ma { menuItems = updateItems <$> zip [0..] (menuItems ma) }
    where
        updateItems (i, mi)
            | i == idx = mi { highlightedColor = Just c }
            | otherwise = mi { highlightedColor = Nothing }

changeScrollHighlight :: ScrollMenuAsset -> Int -> Color -> ScrollMenuAsset
changeScrollHighlight ma idx c
    | idx < 0 = changeScrollHighlight ma 0 c
    | idx >= totalItems = changeScrollHighlight ma (totalItems - 1) c
    | idx < numScrollItems && idx < menuScrollStartIdx ma =
        ma { menuScrollStartIdx = idx
           , menuScrollItems = updateScrollItems <$> zip [0..] (menuScrollItems ma)
           , menuStationaryItems = clearStationary
           }
    | idx < numScrollItems && idx > maxView =
        ma { menuScrollStartIdx = menuScrollStartIdx ma + (idx - maxView)
           , menuScrollItems = updateScrollItems <$> zip [0..] (menuScrollItems ma)
           , menuStationaryItems = clearStationary
           }
    | idx < numScrollItems =
        ma { menuScrollItems = updateScrollItems <$> zip [0..] (menuScrollItems ma)
           , menuStationaryItems = clearStationary
           }
    | otherwise =
        ma { menuScrollItems = clearScroll
           , menuStationaryItems = updateStationaryItems <$> zip [0] (menuStationaryItems ma)
           }
    where
        clearStationary = (\mi -> mi { highlightedColor = Nothing }) <$> menuStationaryItems ma
        clearScroll = (\mi -> mi { highlightedScrollColor = Nothing }) <$> menuScrollItems ma
        maxView = menuScrollStartIdx ma + menuScrollViewable ma
        numScrollItems = length $ menuScrollItems ma
        numEndItems = length $ menuStationaryItems ma
        totalItems = numScrollItems + numEndItems
        updateScrollItems (i, mi)
            | i == idx = mi { highlightedScrollColor = Just c }
            | otherwise = mi { highlightedScrollColor = Nothing }
        updateStationaryItems (i, mi)
            | i == (idx - numScrollItems) = mi { highlightedColor = Just c }
            | otherwise = mi { highlightedColor = Nothing }

changeCursor :: MenuAsset -> Int -> Image -> Double -> MenuAsset
changeCursor ma idx img s = ma { menuItems = updateItems <$> zip [0..] (menuItems ma) }
    where
        updateItems (i, mi)
            | i == idx = mi { cursorImg = Just (img, s) }
            | otherwise = mi { cursorImg = Nothing }

assetObjWidth :: Graphics -> AssetObj -> Int
assetObjWidth gr (AssetText txt _ sz) = floor $ fromIntegral (T.length txt) * fontWidth gr * fromIntegral sz
assetObjWidth gr (AssetImage img scale) = maybe 0 (\i -> floor $ fromIntegral (imageSizeX i) * scale) (M.lookup img (graphicsStaticTextures gr))
assetObjWidth gr (AssetAnimation img _ _ scale) = maybe 0 (\i -> floor $ fromIntegral (animSizeX i) * scale) (M.lookup img (graphicsAnimTextures gr))
assetObjWidth gr (AssetStacked (AssetStack StackVertical items _)) = maximum $ assetObjWidth gr . stackItem <$> items
assetObjWidth gr (AssetStacked (AssetStack StackHorizontal items sp)) = (sum $ (\i -> assetObjWidth gr (stackItem i) + sp) <$> items) - sp
assetObjWidth gr (AssetStacked (AssetStack AbsoluteHorizontal items sp)) = maximum $ (\(idx, i) -> stackXOff i + idx * sp + assetObjWidth gr (stackItem i)) <$> zip [0..] items
assetObjWidth gr (AssetStacked (AssetStack AbsoluteVertical items _)) = maximum $ (\i -> stackXOff i + assetObjWidth gr (stackItem i)) <$> items
assetObjWidth gr (AssetScroll sc) = maximum $ (\ass -> assetObjWidth gr (object ass)) <$> (M.elems $ M.filter (\ass -> isVisible ass) (scrollAssets sc))
assetObjWidth _ (AssetRect w _ _) = w
assetObjWdith _ AssetEmpty = 0


assetObjHeight :: Graphics -> AssetObj -> Int
assetObjHeight gr (AssetText _ _ sz) = ceiling $ fontHeight gr * fromIntegral sz
assetObjHeight gr (AssetImage img scale) = maybe 0 (\i -> floor $ fromIntegral (imageSizeY i) * scale) (M.lookup img (graphicsStaticTextures gr))
assetObjHeight gr (AssetAnimation img _ _ scale) = maybe 0 (\i -> floor $ fromIntegral (animSizeY i) * scale) (M.lookup img (graphicsAnimTextures gr))
assetObjHeight gr (AssetStacked (AssetStack StackVertical items sp))   = (sum $ (\i -> assetObjHeight gr (stackItem i) + sp) <$> items) - sp
assetObjHeight gr (AssetStacked (AssetStack StackHorizontal items _)) = maximum $ (\i -> stackYOff i + assetObjHeight gr (stackItem i)) <$> items
assetObjHeight gr (AssetStacked (AssetStack AbsoluteHorizontal items _)) = maximum $ (\i -> stackYOff i + assetObjHeight gr (stackItem i)) <$> items
assetObjHeight gr (AssetStacked (AssetStack AbsoluteVertical items sp)) = maximum $ (\(idx, i) -> stackYOff i + idx * sp + assetObjHeight gr (stackItem i)) <$> zip [0..] items
assetObjHeight gr (AssetScroll sc) = scrollSeenHeight sc
assetObjHeight _ (AssetRect _ h _) = h
assetObjHeight _ AssetEmpty = 0


getScrollFullHeight :: Graphics -> AssetScroll -> Int
getScrollFullHeight gr (ScrollObj assMap _ _)
    | M.size assMap == 0 = 0
    | otherwise = maximum $ M.map (\(Asset o _ y _ _ _) -> y + assetObjHeight gr o) assMap

getMaxPosition :: Graphics -> AssetScroll -> Int
getMaxPosition gr so@(ScrollObj assMap _ height)
    | M.size assMap == 0 = 0
    | otherwise = max 0 $ maxSc - height
    where
        maxSc = getScrollFullHeight gr so

updateScrollPosition :: Graphics -> AssetScroll -> Int -> AssetScroll
updateScrollPosition gr so@(ScrollObj assMap _ height) newPos  = ScrollObj assMap (min newPos maxPos) height
    where
        maxPos = getMaxPosition gr so


resizeGameView :: a -> Graphics -> GView -> GView
resizeGameView gps gr gv@(GView ats ovs menuAsset) = gv { assets = resizeAssets ats gr, overlays = S.map doOResize ovs, menuAsset = resizeMenu menuAsset gr}
    where
        doOResize over =
            let over' = maybe over (\rsFn -> rsFn over gr) (resizeOverlay over)
            in over' { oAssets = resizeAssets (oAssets over') gr
                     , oMenu = resizeMenu (oMenu over') gr
                     }

resizeMenu :: Maybe AssetMenu -> Graphics -> Maybe AssetMenu
resizeMenu Nothing _ = Nothing
resizeMenu (Just (DefaultMenu menu)) gr = Just $ DefaultMenu $ maybe menu (\rFn -> rFn menu gr) (menuResize menu)
resizeMenu (Just (ScrollMenu menu)) gr = Just $ ScrollMenu $ maybe menu (\rFn -> rFn menu gr) (menuScrollResize menu)


mkScrollMenu :: Graphics -> [ScrollMenuItem] -> [AssetMenuItem] -> (Graphics -> Int) -> Int -> Int -> Int -> Int -> Int -> ScrollMenuAsset
mkScrollMenu gr scItems endItems fromBottom x y sp fs l = ScrollMenuAsset
    { menuScrollX = x
    , menuScrollY = y
    , menuScrollLayer = l
    , menuScrollResize = Just $ scrollMenuResize fromBottom
    , menuScrollLineSpace = sp
    , menuFontSize = fs
    , menuScrollItems = scItems
    , menuScrollStartIdx = 0
    , menuScrollViewable = viewable
    , menuStationaryItems = endItems
    }
    where
        height = graphicsWindowHeight gr - y - fromBottom gr
        entryH = round (fromIntegral fs * fontHeight gr) + sp
        viewable = height `div` entryH

-- bottomSpace computes the vertical space (from Graphics) that should be reserved below the scroll area
scrollMenuResize :: (Graphics -> Int) -> ScrollMenuAsset -> Graphics -> ScrollMenuAsset
scrollMenuResize bottomSpace menu gr = menu { menuScrollStartIdx = newStart, menuScrollViewable = newViewable }
    where
        scItems = menuScrollItems menu
        mv = menuScrollViewable menu
        height = graphicsWindowHeight gr - menuScrollY menu - bottomSpace gr
        entryH = round (fromIntegral (menuFontSize menu) * fontHeight gr) + menuScrollLineSpace menu
        newViewable = height `div` entryH
        cutOff = take (mv - newViewable) (drop (newViewable + menuScrollStartIdx menu) scItems)
        fstHLM = L.findIndex (isJust . highlightedScrollColor) cutOff
        newStart
            | newViewable >= mv = menuScrollStartIdx menu
            | otherwise = case fstHLM of
                            Nothing -> menuScrollStartIdx menu
                            Just idx -> menuScrollStartIdx menu + idx + 1


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
        stack = AssetStacked (AssetStack StackHorizontal (mkTxtAsset <$> tInfo) sp)
        mkTxtAsset (txt, c, sz) = StackItem (AssetText txt c sz) 0 0


oneLineTextRs :: Resize -> [(T.Text, Color, Int)] -> Int -> Int -> Int -> Int -> Asset
oneLineTextRs rsFn tInfo sp x y l = Asset stack x y l True $ Just rsFn
    where
        stack = AssetStacked (AssetStack StackHorizontal (mkTxtAsset <$> tInfo) sp)
        mkTxtAsset (txt, c, sz) = StackItem (AssetText txt c sz) 0 0


-- assumes resize overwrites x and y
mkResizeAsset :: Graphics -> AssetObj -> Int -> Bool -> Resize -> Asset
mkResizeAsset gr obj l vis rFn = rFn (Asset obj undefined undefined l vis (Just rFn)) gr


singleMenu :: Graphics -> T.Text -> Int -> Int -> AssetMenu
singleMenu gr txt yOff l = DefaultMenu $ MenuAsset (txtX gr) (y gr) l (Just menuResize) 0 menuItems
    where
        y gr' = graphicsWindowHeight gr' - ((txtH gr') + yOff)
        menuItems = [MenuItem (MText txt) Blue sz 0 0 True (Just White) Nothing]
        sz = 3
        txtX gr' = midTextStart gr' txt (fromIntegral sz)
        menuResize asset' gr' = asset' { menuXBase = txtX gr', menuYBase = y gr' }
        txtH gr' = ceiling $ fontHeight gr' * fromIntegral sz

nextMenu :: Graphics -> Int -> AssetMenu
nextMenu gr l = singleMenu gr "Next" 50 l

centerAssetX :: Graphics -> AssetObj -> Int -> Int -> Int -> Asset
centerAssetX gr obj xOff y l = mkResizeAsset gr obj l True resize
    where
        resize asset' gr' = asset' { assetX = xMid gr' + xOff, assetY = y }
        xMid gr' = (graphicsWindowWidth gr' - assetObjWidth gr' obj) `div` 2


centerTextX :: Graphics -> T.Text -> Color -> Int -> Int -> Int -> Int -> Asset
centerTextX gr txt c s xOff y l = centerAssetX gr (AssetText txt c s) xOff y l

centerScaleImgX :: Graphics -> T.Text -> Int -> Int -> Int -> Double -> Int -> Asset
centerScaleImgX gr img y xBuff yBuff mScale l = Asset (imgAsset gr) (xStart gr) y l True $ Just rs
    where
        imgInfo gr' = graphicsStaticTextures gr' M.! img
        imgW gr' = fromIntegral $ imageSizeX $ imgInfo gr'
        imgH gr' = fromIntegral $ imageSizeY $ imgInfo gr'
        rs aI gr' = aI { object = imgAsset gr', assetX = xStart gr' }
        imgAsset gr' = AssetImage img (scale gr')
        scaleX gr' = fromIntegral ((graphicsWindowWidth gr') - (xBuff * 2)) / (imgW gr')
        scaleY gr' = fromIntegral ((graphicsWindowHeight gr') - y - xBuff) / (imgH gr')
        scale gr' = max mScale $ min (scaleX gr') (scaleY gr')
        xStart gr' = (graphicsWindowWidth gr' - round (imgW gr' * scale gr')) `div` 2


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

wrapTextStack :: Graphics -> Int -> Color -> T.Text -> Int -> Int -> AssetObj
wrapTextStack gr width c txt sz sp = AssetStacked (AssetStack StackVertical items sp)
    where
        maxChar = floor $ fromIntegral width / (fontWidth gr * fromIntegral sz)
        lines = makeLines txt maxChar
        items = (\t -> StackItem (AssetText t c sz) 0 0) <$> lines

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
        asset gr' = Asset (AssetStacked (AssetStack StackVertical (stack gr') sp)) (xStart gr') startY l True $ Just resize
        resize _ gr' = asset gr'


appendAssetStackCenterX :: Graphics -> StackDir -> Asset -> Int -> Int -> AssetObj -> Asset
appendAssetStackCenterX gr dir baseAsset xOff yOff newObj = appendItem gr baseAsset
    where
        xOffFn gr' parentX = midStartX gr' (assetObjWidth gr' newObj) - parentX + xOff
        appendItem gr' a =
            let stack' = case object a of
                    AssetStacked (AssetStack dir' stack sp)
                        | dir' == dir -> AssetStacked (AssetStack dir' (stack ++ [StackItem newObj (xOffFn gr' (assetX a)) yOff]) sp)
                    obj -> AssetStacked (AssetStack dir [StackItem obj 0 0, StackItem newObj (xOffFn gr' (assetX a)) yOff] 2)
            in a { object = stack', assetResize = newResize }
        newResize = fmap (\fn _ gr' -> appendItem gr' (fn baseAsset gr')) (assetResize baseAsset)


appendAssetStack :: Graphics -> StackDir -> Asset -> Int -> Int -> AssetObj -> Asset
appendAssetStack gr dir baseAsset x yOff newObj = appendItem baseAsset
    where
        appendItem a =
            let xOff = x - assetX a
                stack' = case object a of
                    obj@(AssetStacked (AssetStack d stack sp))
                        | d == dir  -> AssetStacked (AssetStack d (stack ++ [StackItem newObj xOff yOff]) sp)
                        | otherwise -> AssetStacked (AssetStack dir [StackItem obj 0 0, StackItem newObj xOff yOff] sp)
                    obj -> case dir of
                            StackHorizontal -> AssetStacked (AssetStack dir [StackItem obj 0 0, StackItem newObj 0 yOff] xOff)
                            StackVertical -> AssetStacked (AssetStack dir [StackItem obj 0 0, StackItem newObj xOff 0] yOff)
            in a { object = stack', assetResize = combResize }
        combResize = fmap (\fn _ gr' -> appendItem (fn baseAsset gr')) (assetResize baseAsset)


appendAssetStackWith :: Graphics -> StackDir -> Asset -> (Int -> Int -> Graphics -> AssetObj) -> Asset
appendAssetStackWith gr dir baseAsset newObjFn = appendItem gr baseAsset
    where
        appendItem gr' a =
            let newObj = newObjFn (assetX a) (assetY a) gr'
                stack' = case object a of
                    AssetStacked (AssetStack d stack sp)
                        | d == dir  -> AssetStacked (AssetStack d (stack ++ [StackItem newObj 0 0]) sp)
                    obj -> AssetStacked (AssetStack dir [StackItem obj 0 0, StackItem newObj 0 0] 2)
            in a { object = stack', assetResize = combResize }
        combResize = Just $ combResize' (assetResize baseAsset)
        combResize' (Just fn) _ gr' = appendItem gr' $ fn baseAsset gr'
        combResize' _ _ gr' = appendItem gr' baseAsset

getCenterX :: Graphics -> Int -> Int -> Int
getCenterX gr startX width = midStartX gr width - startX
    where
        windowW = graphicsWindowWidth gr

makeTableMenu :: Graphics -> Maybe (MenuAsset -> Graphics -> MenuAsset) -> [(Either T.Text [(T.Text, Maybe Color, Bool)], Int, Color, Maybe Color)] -> Int -> Int -> Int -> Int -> Int -> Int -> AssetMenu
makeTableMenu gr rsFnM rows xMinStart xEndOff xMaxSp yStart ySp layer = DefaultMenu $ MenuAsset
    { menuXBase = startX gr
    , menuYBase = yStart
    , menuLayer = layer
    , menuResize = Just rsFn
    , menuLineSpace = ySp
    , menuItems = mkRows gr
    }
    where
        -- produces a list of the max width of each column
        rowSizes gr' rSizes ((Left _), fs, _, _) = rSizes
        rowSizes gr' rSizes ((Right items), fs, _, _) = rSizes ++ [(\(t, _, _) -> textWidth gr' t fs) <$> items]
        colSizes gr' = maximum <$> (L.transpose $ foldl (rowSizes gr') [] rows)
        space gr' = graphicsWindowWidth gr' - xEndOff - xMinStart
        spacing sizes gr' =
            let n = length sizes
            in if n <= 1 then 1 else max 1 $ (space gr' - sum sizes) `div` (n - 1)
        colOffsets sizes gr' =
            let sp = spacing sizes gr'
            in scanl (\xOff w -> xOff + w + sp) 0 $ init sizes
        startX gr' =
            let sizes = colSizes gr'
                sp = spacing sizes gr'
                totalWidth = (sum sizes) + (sp * max 0 (length sizes - 1))
            in xMinStart + (space gr' - totalWidth) `div` 2
        mkItems _ (Left t, fs, bc, hlc)  = MenuItem (MText t) bc fs 0 0 True hlc Nothing
        mkItems colOffs (Right items, fs, bc, hlc) = MenuItem (MRow (zipWith (\(t, mc, b) off -> (t, off, mc, b)) items colOffs)) bc fs 0 0 True hlc Nothing
        mkRows gr' =
            let sizes = colSizes gr'
            in (mkItems (colOffsets sizes gr')) <$> rows
        rsFn ma gr' =
            let rowSizes fs prev (MText _) = prev
                rowSizes fs prev (MRow mis) = prev ++ [(\(t, _, _, _) -> textWidth gr' t fs) <$> mis]
                sizes = maximum <$> (L.transpose $ foldl (\acc mi -> rowSizes (menuItemFontSize mi) acc (menuItemText mi)) [] (menuItems ma))
                offsets = colOffsets sizes gr'
                ma' = ma { menuXBase = startX gr', menuItems = updateMenuRow offsets <$> menuItems ma }
            in maybe ma' (\f -> f ma' gr') rsFnM


updateMenuRow :: [Int] -> AssetMenuItem -> AssetMenuItem
updateMenuRow offsets mi@(MenuItem (MRow row) _ _ _ _ _ _ _) =
    mi { menuItemText = MRow (zipWith (\(t, _, mc, b) off -> (t, off, mc, b)) row offsets) }
updateMenuRow _ mi = mi
