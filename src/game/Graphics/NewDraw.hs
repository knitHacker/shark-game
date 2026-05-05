module Graphics.NewDraw
    ( drawAssets
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

import Graphics.Types
import Graphics.NewTypes
import Graphics.Asset
import Graphics.TextUtil
import OutputHandles.Types
import OutputHandles.Images
import OutputHandles.Text

drawAssets :: Graphics -> GView -> ToRender
drawAssets gr gv = r''
    where
        baseView = foldl (drawAsset gr 0) mempty (assets gv)

        appendMenu r Nothing = r
        appendMenu r (Just mAss) = fst $ drawMenu gr 0 r mAss
        r' = appendMenu baseView $ menuAsset gv
        r'' = if null (activeOverlays gv) then r' else foldl appendOverlay r' $ activeOverlays gv
        appendOverlay rNew idx = drawOverlay gr (overlays gv M.! idx) (idx + 1) rNew
        --TODO: Add overlays

drawOverlay :: Graphics -> Overlay -> Int -> ToRender -> ToRender
drawOverlay gr (AOverlay assets menu ox oy ow oh oc _) d r = r'''
    where
        r' = addRectangle r d 0 $ DRectangle oc (fromIntegral ox) (fromIntegral oy) (fromIntegral ow) (fromIntegral oh)
        r'' = foldl (drawAsset gr d) r' assets
        r''' = maybe r'' (fst . drawMenu gr d r'') menu

getMax :: StackDir -> Graphics -> Int -> Int -> AssetObj -> Int
getMax StackHorizontal gr x y asset = x + assetObjWidth gr asset
getMax StackVertical gr x y asset = y + assetObjHeight gr asset

getTexture :: Graphics -> Image -> Int -> Int -> Double -> DrawTexture
getTexture gr img x y s = DTexture img (fromIntegral x) (fromIntegral y) w h Nothing
    where
        iInfo = graphicsStaticTextures gr M.! img
        w = floor $ fromIntegral (imageSizeX iInfo) * s
        h = floor $ fromIntegral (imageSizeY iInfo) * s

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
                    AssetStacked dir ais sp -> fst $ addStack dir gr d r l sp x y ais
                    _ -> r

addStack :: StackDir -> Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> [AssetStackItem] -> (ToRender, Int)
addStack StackHorizontal gr d r l sp x y [] = (r, x)
addStack StackVertical gr d r l sp x y [] = (r, y)
addStack dir gr d r l sp x y (h:tl) =
    let (r', end) = drawStack dir gr d r l sp x y h
    in case dir of
        StackHorizontal -> addStack dir gr d r' l sp end y tl
        StackVertical -> addStack dir gr d r' l sp x end tl

drawStack :: StackDir -> Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> AssetStackItem -> (ToRender, Int)
drawStack dir gr d r l sp x y (StackItem item xOff yOff) =
    case item of
        AssetImage img s ->
            let draw = getTexture gr img (x + xOff) (y + yOff) s
            in (addTexture r d l draw, end + sp)
        AssetAnimation an fr dp s ->
            let draw = getAnimFrame gr an fr dp x (y + yOff) s
            in (addAnimTexture r d l draw, end + sp)
        AssetText txt c s ->
            let draw = TextDisplay txt (fromIntegral (x + xOff)) (fromIntegral (y + yOff)) s c Nothing
            in (addText r d l draw, end + sp)
        AssetRect w h c ->
            let draw = DRectangle c (fromIntegral (x + xOff)) (fromIntegral (y + yOff)) (fromIntegral w) (fromIntegral h)
            in (addRectangle r d l draw, end + sp)
        AssetScroll scroll -> undefined
        AssetStacked dir' stack sp' ->
            let (r', _) = addStack dir' gr d r l sp' (x + xOff) (y + yOff) stack
            in (r', end + sp)
    where
        end = getMax dir gr (x + xOff) (y + yOff) item

-- TODO: Add the scroll bar draw logic
-- probably needs start, end, bar start, bar end
drawScrollBar :: ToRender -> ToRender
drawScrollBar r = r

drawMenu :: Graphics -> Int -> ToRender -> MenuAsset -> (ToRender, Int)
drawMenu gr d r (MenuAsset x y l _ scrollB sp items) = foldl foldItem (r', y) items
    where
        r' = if scrollB then drawScrollBar r else r
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
        yAdj = max 2 (min (floor (fromIntegral (sp - 1) / 2)) (fromIntegral (div (textH - 1) 2)))
        x' = fromIntegral $ x - xAdj
        y' = fromIntegral $ y - yAdj
        w = fromIntegral $ textL + (2 * xAdj)
        h = fromIntegral $ textH + (2 * yAdj)

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


drawMenuItem :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> AssetMenuItem -> (ToRender, Int)
drawMenuItem gr d r l sp x y (MenuItem txt c sz xOff yOff hlM csrM) = (addText r'' d (l + 1) draw, maxY + sp)
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


