{-# LANGUAGE OverloadedStrings #-}

module Handles
    ( Handles(..)
    , initHandles
    , cleanupHandles
    , getWindowSize
    , getFontSize
    ) where


import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Image
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Configs
import Draw.Types
import Handles.Types
import Env.Files
import Draw

initWindow :: SDL.Renderer -> IO ()
initWindow r = do
    setColor r Black
    SDL.clear r
    SDL.present r

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedRenderer
  , SDL.rendererTargetTexture = False
  }

-- Get the font size
-- Only works because the font is monospaced
getFontSize :: Handles -> IO FontSize
getFontSize outs =
    do
        size <-Font.size (font outs) " "
        let size' = (fromIntegral (fst size), fromIntegral (snd size))
        return size'

getWindowSize :: Handles -> IO (Int, Int)
getWindowSize outs = do
    (SDL.V2 w h) <- SDL.get (SDL.windowSize (window outs))
    return (fromIntegral w, fromIntegral h)


loadTexture :: SDL.Renderer -> (T.Text, FilePath) -> IO (T.Text, SDL.Texture)
loadTexture r (name, filePath) = do
    path <- getGameFullPath filePath
    t <- SDL.Image.loadTexture r path
    return (name, t)


initHandles :: TextureCfg -> GameConfigs -> IO Handles
initHandles textCfgs cfgs = do
    fontPath <- getGameFullPath fontFile
    SDL.initialize []
    window <- SDL.createWindow "Shark Game" SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        , SDL.windowHighDPI = True
        , SDL.windowMode = if fs then SDL.FullscreenDesktop else SDL.Windowed
        , SDL.windowResizable = True
        }
    SDL.showWindow window
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    initWindow renderer
    font <- Font.load fontPath fontSz
    textList <- mapM (loadTexture renderer) $ M.toList $ getTextureFiles textCfgs
    let textures = M.fromList textList
    return $ Handles
        { window = window
        , renderer = renderer
        , textures = textures
        , font = font
        }

    where
        gCfgs = settingCfgs cfgs
        fontSz = fontSize gCfgs
        fs = fullScreen gCfgs
        screenWidth = fromIntegral $ windowSizeX gCfgs
        screenHeight = fromIntegral $ windowSizeY gCfgs


cleanupHandles :: Handles -> IO ()
cleanupHandles outs = do
    Font.free $ font outs
    Font.quit
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit
