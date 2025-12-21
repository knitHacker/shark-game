module Graphics.ImageUtil
    ( scalingRecenterImage
    ) where

import qualified Data.Map.Strict as M

import OutputHandles.Types

import Graphics.Types

-- This will scale so the image fits between the start x and the right side of the window,
-- centering it in that space, and return the ImagePlacement along with the x and scale used.
scalingRecenterImage :: Graphics -> Int -> Int -> Double -> Image -> (ImagePlacement, Int, Double)
scalingRecenterImage gr xStart yPos minScale imageName = (image, iX, scale)
    where
        windowWidth = graphicsWindowWidth gr
        halfWidth = windowWidth - xStart
        windowHeight = graphicsWindowHeight gr
        ii = graphicsStaticTextures gr M.! imageName
        imageBufferW = 40
        imageBufferH = 300
        iSpaceWidth = halfWidth - imageBufferW
        iSpaceHeight = windowHeight - imageBufferH
        scaleX = fromIntegral iSpaceWidth / fromIntegral (imageSizeX ii)
        scaleY = fromIntegral iSpaceHeight / fromIntegral (imageSizeY ii)
        -- have a minimum scale so it doesn't get too small on tiny windows
        -- otherwise scale to fit in the space in both x and y
        scale = max minScale (min scaleX scaleY)
        -- The place where the menu text ends
        -- This will center it in the right "half" of the screen
        iX = xStart + (halfWidth - round (fromIntegral (imageSizeX ii) * scale)) `div` 2
        iY = 250
        image = IPlace iX iY scale imageName 1
        -- scale the offset of the start of the flag animation to put it in the right place
        flagOffsetX = round (21 * scale)
        flagOffsetY = round (132 * scale)