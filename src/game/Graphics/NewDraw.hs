module Graphics.NewDraw
    ( drawAssets
    ) where

import qualified Data.Map.Strict as M

import Graphics.Types
import Graphics.NewTypes
import OutputHandles.Types
import OutputHandles.Images
import OutputHandles.Text

drawAssets :: Graphics -> GView a -> ToRender
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

drawAsset :: Graphics -> Int -> ToRender -> Asset a -> ToRender
drawAsset gr d r (Asset o x y s l isVis _ _ _)
    | not isVis = r
    | otherwise = case o of
                    AssetImage i -> addTexture r d l $ getTexture gr i x y s
                    AssetAnimation an fr dp -> addAnimTexture r d l $ getAnimFrame gr an fr dp x y s
                    AssetText txt c -> addText r d l $ TextDisplay txt (fromIntegral x) (fromIntegral y) (floor s) c Nothing
                    AssetRect w h c -> addRectangle r d l $ DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                    _ -> r
