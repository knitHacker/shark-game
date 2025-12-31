module Graphics.ImageUtil
    ( scalingRecenterImage
    , centerScalingImage
    , centerImage
    , centerAnimation
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types

import Graphics.Types

-- This will scale so the image fits between the start x and the right side of the window,
-- centering it in that space, and return the ImagePlacement along with the x and scale used.
scalingRecenterImage :: Graphics -> Int-> Int -> Int -> Int -> Double -> Image -> (ImagePlacement, Int, Double)
scalingRecenterImage gr xStart iY xEndBuf yEndBuff minScale imageName = (image, iX, scale)
    where
        windowWidth = graphicsWindowWidth gr
        halfWidth = windowWidth - xStart
        windowHeight = graphicsWindowHeight gr
        ii = graphicsStaticTextures gr M.! imageName
        iSpaceWidth = halfWidth - xEndBuf
        iSpaceHeight = windowHeight - iY - yEndBuff
        scaleX = fromIntegral iSpaceWidth / fromIntegral (imageSizeX ii)
        scaleY = fromIntegral iSpaceHeight / fromIntegral (imageSizeY ii)
        -- have a minimum scale so it doesn't get too small on tiny windows
        -- otherwise scale to fit in the space in both x and y
        scale = max minScale (min scaleX scaleY)
        -- The place where the menu text ends
        -- This will center it in the right "half" of the screen
        iWidth = round (fromIntegral (imageSizeX ii) * scale)
        iX = max xStart $ xStart + (halfWidth - iWidth) `div` 2
        image = IPlace iX iY scale imageName 1


-- This will scale so the image fits between the start x and the right side of the window,
-- centering it in that space, and return the ImagePlacement along with the x and scale used.
centerScalingImage :: Graphics -> Int -> Int -> Int -> Double -> Image -> (ImagePlacement, Int, Int, Double)
centerScalingImage gr iY xBuff yBuff minScale imageName = (image, iX, iY + iHeight, scale)
    where
        windowWidth = graphicsWindowWidth gr
        windowHeight = graphicsWindowHeight gr
        ii = graphicsStaticTextures gr M.! imageName
        iSpaceWidth = windowWidth - (xBuff * 2)
        iSpaceHeight = windowHeight - iY - yBuff
        scaleX = fromIntegral iSpaceWidth / fromIntegral (imageSizeX ii)
        scaleY = fromIntegral iSpaceHeight / fromIntegral (imageSizeY ii)
        -- have a minimum scale so it doesn't get too small on tiny windows
        -- otherwise scale to fit in the space in both x and y
        scale = max minScale (min scaleX scaleY)
        -- The place where the menu text ends
        -- This will center it in the right "half" of the screen
        iWidth = round (fromIntegral (imageSizeX ii) * scale)
        iHeight = round (fromIntegral (imageSizeY ii) * scale)
        iX = (windowWidth - iWidth) `div` 2
        image = IPlace iX iY scale imageName 1
        -- scale the offset of the start of the flag animation to put it in the right place
        flagOffsetX = round (21 * scale)
        flagOffsetY = round (132 * scale)


centerImage :: Graphics -> Int -> Double -> Image -> ImagePlacement
centerImage gr yPos scale imageName = image
    where
        windowWidth = graphicsWindowWidth gr
        ii = graphicsStaticTextures gr M.! imageName
        iX = (windowWidth - round (fromIntegral (imageSizeX ii) * scale)) `div` 2
        image = IPlace iX yPos scale imageName 1


centerAnimation :: Graphics -> Int -> Double -> Image -> AnimPlacement
centerAnimation gr yPos scale animName = animation
    where
        windowWidth = graphicsWindowWidth gr
        ai = graphicsAnimTextures gr M.! animName
        iX = (windowWidth - round (fromIntegral (animSizeX ai) * scale)) `div` 2
        animation = APlace iX yPos scale animName 0 0 1 True