module Graphics.Draw
    ( drawAssets
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe)

import Graphics.Types
import Graphics.Asset
import Graphics.TextUtil
import OutputHandles.Types
import OutputHandles.Images
import OutputHandles.Text
import OutputHandles.Util

drawAssets :: Graphics -> GView -> ToRender
drawAssets gr gv = r''
    where
        baseView = foldl (drawAsset gr 0) mempty (assets gv)
        appendMenu r Nothing = r
        appendMenu r (Just mAss) = fst $ drawMenu gr 0 r mAss
        r' = appendMenu baseView $ menuAsset gv
        r'' = if null (overlays gv) then r' else foldl appendOverlay r' $ zip [1..] $ S.toList $ overlays gv
        appendOverlay rNew (idx, ov) = drawOverlay gr ov (idx + 1) rNew

drawOverlay :: Graphics -> Overlay -> Int -> ToRender -> ToRender
drawOverlay gr (AOverlay _ assets menu ox oy ow oh oc _) d r = r'''
    where
        r' = addRectangle r d 0 $ DRectangle oc (fromIntegral ox) (fromIntegral oy) (fromIntegral ow) (fromIntegral oh)
        r'' = foldl (drawAsset gr d) r' assets
        r''' = maybe r'' (fst . drawMenu gr d r'') menu

getMax :: Bool -> Graphics -> Int -> Int -> AssetObj -> Int
getMax True gr x y asset = x + assetObjWidth gr asset
getMax False gr x y asset = y + assetObjHeight gr asset

getMaxes :: Graphics -> Int -> Int -> AssetObj -> (Int, Int)
getMaxes gr x y asset = (x + assetObjWidth gr asset, y + assetObjHeight gr asset)

getTexture :: Graphics -> Image -> Int -> Int -> Double -> DrawTexture
getTexture gr img x y s = DTexture img (fromIntegral x) (fromIntegral y) w h Nothing
    where
        iInfo = graphicsStaticTextures gr M.! img
        w = floor $ fromIntegral (imageSizeX iInfo) * s
        h = floor $ fromIntegral (imageSizeY iInfo) * s

getTexturePartial :: Graphics -> Image -> Int -> Int -> Double -> (Int, Int, Int, Int) -> DrawTexture
getTexturePartial gr img x y s (mx, my, wOff, hOff) = partialTexture img x y w h (mx, my, w + wOff, h + hOff)
    where
        iInfo = graphicsStaticTextures gr M.! img
        w = floor $ fromIntegral (imageSizeX iInfo) * s
        h = floor $ fromIntegral (imageSizeY iInfo) * s

getTextPartial :: Graphics -> T.Text -> Color -> Int -> Int -> Int -> (Int, Int, Int, Int) -> TextDisplay
getTextPartial gr txt c sz x y (mx, my, wOff, hOff) = partialText txt c sz x y (mx, my, w + wOff, h + hOff)
    where
        w = floor $ fromIntegral (T.length txt) * (fontWidth gr) * fromIntegral sz
        h = floor $ fontHeight gr * fromIntegral sz

getAnimFrame :: Graphics -> Image -> Int -> Int -> Int -> Int -> Double -> DrawAnimFrame
getAnimFrame gr img fr dp x y s = DFrame img (fromIntegral x) (fromIntegral y) w h s fr dp
    where
        iInfo = graphicsAnimTextures gr M.! img
        w = fromIntegral $ animSizeX iInfo
        h = fromIntegral $ animSizeY iInfo


drawAsset :: Graphics -> Int -> ToRender -> Asset -> ToRender
drawAsset gr d r (Asset o x y l isVis _)
    | not isVis = r
    | otherwise = case o of
                    AssetImage i s -> addTexture r d l $ getTexture gr i x y s
                    AssetAnimation an fr dp s -> addAnimTexture r d l $ getAnimFrame gr an fr dp x y s
                    AssetText txt c s -> addText r d l $ TextDisplay txt (fromIntegral x) (fromIntegral y) s c Nothing
                    AssetRect w h c -> addRectangle r d l $ DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                    AssetStacked (AssetStack dir ais sp) -> fst $ addStack dir gr d r l sp x y ais
                    AssetScroll sc -> addScroll gr r d l x y sc
                    _ -> r


addScroll :: Graphics -> ToRender -> Int -> Int -> Int -> Int -> AssetScroll -> ToRender
addScroll gr r d l x y (ScrollObj assMap pos height) = r''
    where
        positions = M.elems $ M.mapWithKey (\k (Asset o x y _ _ _) -> (y, y + assetObjHeight gr o, k)) (M.filter (\a -> isVisible a) assMap)
        seenOriginal = filter (\(yStart, yEnd, _) -> yEnd > pos && yStart < height + pos) positions
        seen = (\(s, e, i) -> (s - pos, e - pos, i)) <$> seenOriginal
        scrollMax = if M.size assMap == 0 then 0 else maximum $ M.map (\(Asset o _ y _ _ _) -> y + assetObjHeight gr o) assMap
        r' = foldl scrollAddFold r seen
        scrollAddFold rend (s, e, i) = addScrollItem gr rend d l height x y (assMap M.! i) (s, e)
        scrollRatio = fromIntegral height / fromIntegral scrollMax
        scrollPos = round $ fromIntegral pos * scrollRatio
        scrollSubH = floor $ scrollRatio * fromIntegral height
        r'' = if scrollMax <= height
                then r'
                else drawScrollBar r' d l (x - 40) y height scrollPos scrollSubH

addScrollItem :: Graphics -> ToRender -> Int -> Int -> Int -> Int -> Int -> Asset -> (Int, Int) -> ToRender
addScrollItem gr r d l h x y (Asset obj sx sy sl _ _) (s, e)
    | s >= 0 && h >= e = drawAsset gr d r (Asset obj (sx + x) (s + y) (l + sl) True Nothing)
    | otherwise =
        case obj of
            AssetImage i scale -> addTexture r d (l + sl) $ getTexturePartial gr i (x + sx) (y + s) scale (0, yRelative, 0, hRelative)
            AssetText txt c sz -> addText r d (l + sl) $ getTextPartial gr txt c sz (x + sx) (y + s) (x + sx, y + s + yRelative, 0, hRelative)
            AssetRect w h c -> addRectangle r d (l + sl) $ DRectangle c (fromIntegral (x + sx)) (fromIntegral (y + s + yRelative)) (fromIntegral w) (fromIntegral (h + hRelative))
            AssetStacked (AssetStack StackVertical stackItems spacing) ->
                foldl renderSub r subInfos
                where
                    subInfos = snd $ foldl accum (0, []) stackItems
                    accum (yAcc, acc) (StackItem subObj subXOff subYOff) =
                        let subStart = yAcc + subYOff
                            subEnd   = subStart + assetObjHeight gr subObj
                        in (subEnd + spacing, acc ++ [(subObj, subXOff, subStart)])
                    renderSub rend (subObj, subXOff, subYStart) =
                        let s' = s + subYStart
                            e' = s' + assetObjHeight gr subObj
                        in if s' >= h || e' <= 0 then rend
                           else addScrollItem gr rend d l h x y (Asset subObj (sx + subXOff) 0 sl True Nothing) (s', e')
            AssetStacked (AssetStack AbsoluteVertical stackItems spacing) ->
                foldl renderSub r (zip [0..] stackItems)
                where
                    renderSub rend (i, StackItem subObj subXOff subYOff) =
                        let s' = s + i * spacing + subYOff
                            e' = s' + assetObjHeight gr subObj
                        in if s' >= h || e' <= 0 then rend
                           else addScrollItem gr rend d l h x y (Asset subObj (sx + subXOff) 0 sl True Nothing) (s', e')
            AssetStacked _ -> drawAsset gr d r (Asset obj (sx + x) (s + y) (l + sl) True Nothing)
            AssetScroll sc -> addScroll gr r d (l + sl) (sx + x) (s + y) sc
            -- AssetAnimation has no partial clip support in DrawAnimFrame; skip boundary clipping for now
            AssetAnimation _ _ _ _  -> r
            AssetEmpty -> r
    where
        topCutOff = if s < 0 then s else 0
        botCutOff = if e > h then - (e - h) else 0
        yRelative = if s < 0 then -s else 0
        hRelative = topCutOff + botCutOff

addStack :: StackDir -> Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> [AssetStackItem] -> (ToRender, Int)
addStack dir gr d r l sp x y ls =
    case dir of
        StackHorizontal ->
            let (r', _, end) = foldl drawSH (r, x, x) ls
            in (r', end)
        StackVertical ->
            let (r', _, end) = foldl drawSV (r, y, y) ls
            in (r', end)
        AbsoluteHorizontal ->
            let (r', _, end) = foldl drawAH (r, 0, x) ls
            in (r', end)
        AbsoluteVertical ->
            let (r', _, end) = foldl drawAV (r, 0, y) ls
            in (r', end)
    where
        drawSH (r', end, _) si@(StackItem obj xOff _) =
            let end' = end + xOff + assetObjWidth gr obj
            in (drawStack gr d r' l end y si, end' + sp, end')
        drawSV (r', end, _) si@(StackItem obj _ yOff) =
            let end' = end + yOff + assetObjHeight gr obj
            in (drawStack gr d r' l x end si, end' + sp, end')
        drawAH (r', sp', end) si@(StackItem obj xOff _) =
            (drawStack gr d r' l (x + sp') y si, sp' + sp, x + sp' + xOff + assetObjWidth gr obj)
        drawAV (r', sp', end) si@(StackItem obj _ yOff) =
            (drawStack gr d r' l x (y + sp') si, sp' + sp, y + sp' + yOff + assetObjHeight gr obj)

drawStack :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> AssetStackItem -> ToRender
drawStack gr d r l x y (StackItem item xOff yOff) =
    case item of
        AssetImage img s -> addTexture r d l $ getTexture gr img (x + xOff) (y + yOff) s
        AssetAnimation an fr dp s -> addAnimTexture r d l $ getAnimFrame gr an fr dp x (y + yOff) s
        AssetText txt c s -> addText r d l $ TextDisplay txt (fromIntegral (x + xOff)) (fromIntegral (y + yOff)) s c Nothing
        AssetRect w h c -> addRectangle r d l $ DRectangle c (fromIntegral (x + xOff)) (fromIntegral (y + yOff)) (fromIntegral w) (fromIntegral h)
        AssetScroll scroll -> undefined
        AssetStacked (AssetStack dir stack sp) -> fst $ addStack dir gr d r l sp (x + xOff) (y + yOff) stack

-- TODO: Add the scroll bar draw logic
drawScrollBar :: ToRender -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ToRender
drawScrollBar r d baseL startX startY height offSet subH = r''
    where
        w = 20
        r' = addRectangle r d baseL $ DRectangle DarkGray (fromIntegral startX) (fromIntegral startY) w (fromIntegral height)
        r'' = addRectangle r' d (baseL + 1) $ DRectangle Gray (fromIntegral startX) (fromIntegral (startY + offSet)) w (fromIntegral subH)

drawMenu :: Graphics -> Int -> ToRender -> AssetMenu -> (ToRender, Int)
drawMenu gr d r (ScrollMenu (ScrollMenuAsset x y l _ sp sz items startIdx viewable stationary)) = (r''', max scrollBottom stationaryBottom)
    where
        visible     = take viewable $ drop startIdx items
        numTotal    = length items
        itemHeight  = ceiling (fontHeight gr * fromIntegral sz) + sp
        shownHeight = viewable * itemHeight
        barOffset   = if numTotal == 0 then 0
                      else floor $ fromIntegral shownHeight * fromIntegral startIdx / fromIntegral numTotal
        subH        = if numTotal == 0 then shownHeight
                      else floor $ fromIntegral shownHeight * fromIntegral viewable / fromIntegral numTotal
        r'          = if numTotal <= viewable then r
                      else drawScrollBar r d (l + 1) (x - 40) y shownHeight barOffset subH
        (r'', scrollBottom)      = foldl (\(rNew, yNew) mi -> drawScrollMenuItem gr d rNew l sz sp x yNew mi) (r', y) visible
        (r''', stationaryBottom) = foldl (\(rNew, yNew) mi -> drawMenuItem gr d rNew l sp x yNew mi) (r'', scrollBottom + sp) stationary
drawMenu gr d r (DefaultMenu (MenuAsset x y l _ sp items)) = foldl foldItem (r, y) items
    where
        foldItem (rNew, y') menuItem = drawMenuItem gr d rNew l sp x y' menuItem

-- Make a rectangle around text in the given space
getTextRectangle :: Graphics -> Color -> Int -> Int -> Int -> Int -> Int -> DrawRectangle
getTextRectangle gr c txtLen x y sz sp = DRectangle c x' y' w h
    where
        fontW = fontWidth gr
        fontH = fontHeight gr
        textL = ceiling $ fontW * (fromIntegral txtLen) * (fromIntegral sz)
        textH = ceiling $ fontH * (fromIntegral sz)
        xAdj = max 1 (fromIntegral sz)
        yAdj = max 2 (min (floor (fromIntegral sp / 2)) (fromIntegral (div (textH - 2) 2)))
        x' = fromIntegral $ x - xAdj
        y' = fromIntegral $ y - yAdj
        w = fromIntegral $ textL + (2 * xAdj)
        h = fromIntegral $ textH + (2 * yAdj) - 2

getCursor :: Graphics -> Image -> Double -> Int -> Int -> Int -> DrawTexture
getCursor gr img iS x y sz = DTexture img x' y' (floor curW) (floor curH) Nothing
    where
        iInfo = graphicsStaticTextures gr M.! img
        txtH = fontHeight gr * fromIntegral sz
        yMid = fromIntegral y + (txtH / 2.0)
        fw = fontWidth gr * fromIntegral sz
        curW = (fromIntegral (imageSizeX iInfo)) * iS
        curH = (fromIntegral (imageSizeY iInfo)) * iS
        x' = floor $ (fromIntegral x) - curW - fw
        y' = floor $ yMid - (curH / 2.0) - 5

drawScrollMenuItem :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> Int -> ScrollMenuItem -> (ToRender, Int)
drawScrollMenuItem gr d r l sz sp x y (ScrollMenuItem (MText txt) c xOff hlM imgM) = (addText r'' d (l + 1) draw, maxY + sp)
    where
        x'   = x + xOff
        r'   = case hlM of
                   Nothing -> r
                   Just rc -> addRectangle r d l $ getTextRectangle gr rc (T.length txt) x' y sz sp
        r''  = case imgM of
                   Nothing -> r'
                   Just (csr, iS) -> addTexture r' d l $ getCursor gr csr iS x' y sz
        draw = TextDisplay txt (fromIntegral x') (fromIntegral y) sz c Nothing
        maxY = fromJust $ getTextMaxY (graphicsFontSize gr) [draw]
drawScrollMenuItem gr d r l sz sp x y (ScrollMenuItem (MRow cells) c xOff hlM imgM) = (r''', maxY + sp)
    where
        x'   = x + xOff
        r'   = case imgM of
                   Nothing -> r
                   Just (csr, iS) -> addTexture r d l $ getCursor gr csr iS x' y sz
        r''' = foldl drawCell r' cells
        drawCell rAcc (txt, cxOff, colorM, hl) =
            let cx    = x' + cxOff
                cc    = fromMaybe c colorM
                rAcc' = case (hlM, hl) of
                            (Just rc, True) -> addRectangle rAcc d l $ getTextRectangle gr rc (T.length txt) cx y sz sp
                            _ -> rAcc
            in addText rAcc' d (l + 1) $ TextDisplay txt (fromIntegral cx) (fromIntegral y) sz cc Nothing
        maxY = case cells of
                   (txt, _, _, _):_ -> fromJust $ getTextMaxY (graphicsFontSize gr) [TextDisplay txt (fromIntegral x') (fromIntegral y) sz c Nothing]
                   [] -> y

drawMenuItem :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> AssetMenuItem -> (ToRender, Int)
drawMenuItem _ _ r _ _ _ y (MenuItem _ _ _ _ _ False _ _) = (r, y)
drawMenuItem gr d r l sp x y (MenuItem (MText txt) c sz xOff yOff True hlM csrM) = (addText r'' d (l + 1) draw, maxY + sp)
    where
        x' = x + xOff
        y' = y + yOff
        r' = case hlM of
                Nothing -> r
                Just rc -> addRectangle r d l $ getTextRectangle gr rc (T.length txt) x' y' sz sp
        r'' = case csrM of
                Nothing -> r'
                Just (csr, iS) -> addTexture r' d l $ getCursor gr csr iS x' y' sz
        draw = TextDisplay txt (fromIntegral x') (fromIntegral y') sz c Nothing
        maxY = fromJust $ getTextMaxY (graphicsFontSize gr) [draw]
drawMenuItem gr d r l sp x y (MenuItem (MRow cells) c sz xOff yOff True hlM csrM) = (r''', maxY + sp)
    where
        x' = x + xOff
        y' = y + yOff
        r' = case csrM of
                Nothing -> r
                Just (csr, iS) -> addTexture r d l $ getCursor gr csr iS x' y' sz
        r''' = foldl drawCell r' cells
        drawCell rAcc (txt, cxOff, colorM, hl) =
            let cx = x' + cxOff
                cc = fromMaybe c colorM
                rAcc' = case (hlM, hl) of
                            (Just rc, True) -> addRectangle rAcc d l $ getTextRectangle gr rc (T.length txt) cx y' sz sp
                            _ -> rAcc
            in addText rAcc' d (l + 1) $ TextDisplay txt (fromIntegral cx) (fromIntegral y') sz cc Nothing
        maxY = case cells of
                   (txt, _, _, _):_ -> fromJust $ getTextMaxY (graphicsFontSize gr) [TextDisplay txt (fromIntegral x') (fromIntegral y') sz c Nothing]
                   [] -> y'
