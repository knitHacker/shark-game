module Graphics.NewDraw
    ( drawAssets
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

import Graphics.Types
import Graphics.NewTypes
import Graphics.TextUtil
import OutputHandles.Types
import OutputHandles.Images
import OutputHandles.Text

drawAssets :: Graphics -> GView -> ToRender
drawAssets gr gv = baseView
    where
        baseView = foldl (drawAsset gr 0) mempty (assets gv)
        --TODO: Add overlays


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
                    AssetStacked ais sp -> fst $ addStack gr d r l sp x y ais
                    AssetMenu menu -> fst $ drawMenu gr d r l x y menu
                    _ -> r

addStack :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> [AssetStackItem] -> (ToRender, Int)
addStack gr d r l sp x y [] = (r, y)
addStack gr d r l sp x y (h:tl) =
    let (r', endY) = drawStack gr d r l sp x y h
    in addStack gr d r' l sp x endY tl

drawStack :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> AssetStackItem -> (ToRender, Int)
drawStack gr d r l sp x y (StackItem item xOff) =
    case item of
        AssetImage img s ->
            let draw = getTexture gr img (x + xOff) y s
                endY = getImageMaxY [draw]
            in (addTexture r d l draw, endY + sp)
        AssetAnimation an fr dp s ->
            let draw = getAnimFrame gr an fr dp x y s
                endY = getAnimMaxY [draw]
            in (addAnimTexture r d l draw, endY + sp)
        AssetText txt c s ->
            let draw = TextDisplay txt (fromIntegral (x + xOff)) (fromIntegral y) s c Nothing
                endY = fromJust $ getTextMaxY (graphicsFontSize gr) [draw]
            in (addText r d l draw, endY + sp)
        AssetRect w h c ->
            let draw = DRectangle c (fromIntegral (x + xOff)) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                endY = getRectMaxY [draw]
            in (addRectangle r d l draw, endY + sp)
        AssetScroll scroll -> undefined
        AssetStacked stack sp' ->
            let (r', endY) = addStack gr d r l sp' x y stack
            in (r', endY + sp)
        AssetMenu menu -> drawMenu gr d r l x y menu

-- TODO: Add the scroll bar draw logic
-- probably needs start, end, bar start, bar end
drawScrollBar :: ToRender -> ToRender
drawScrollBar r = r

drawMenu :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> AssetMenu -> (ToRender, Int)
drawMenu gr d r l x y (MenuObj items scrollB sp) = foldl foldItem (r', y) items
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
        y' = floor $ yMid - (curH / 2.0)


drawMenuItem :: Graphics -> Int -> ToRender -> Int -> Int -> Int -> Int -> AssetMenuItem -> (ToRender, Int)
drawMenuItem gr d r l sp x y (MenuItem txt c sz hlM csrM) = (addText r'' d l draw, maxY + sp)
    where
        r' = case hlM of
                Nothing -> r
                Just rc -> addRectangle r d l $ getTextRectangle gr rc (T.length txt) x y sz sp
        r'' = case csrM of
                Nothing -> r'
                Just (csr, iS) -> addTexture r d l $ getCursor gr csr iS x y sz
        draw = TextDisplay txt (fromIntegral x) (fromIntegral y) sz c Nothing
        maxY = fromJust $ getTextMaxY (graphicsFontSize gr) [draw]


