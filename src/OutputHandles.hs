{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
module OutputHandles
    ( initOutputHandles
    , cleanupOutputHandles
    , executeDraw
    , getFontSize
    , getWindowSize
    ) where


import Foreign.C.Types ()
import qualified SDL
import qualified SDL.Image
import SDL.Vect ( V2(V2) )
import SDL                    (($=))
import qualified SDL.Font as Font
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory ()
import Data.Maybe (catMaybes)

import Paths_shark_game ()
import Configs
import OutputHandles.Types
import OutputHandles.Draw
import Env.Files            (getGameFullPath)


import Debug.Trace

fontFile :: FilePath
fontFile = "assets/fonts/saxmono.ttf"
--fontFile = "assets/fonts/InsightSansSSi.ttf"

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedRenderer
  , SDL.rendererTargetTexture = False
  }

-- Get the font size
-- Only works because the font is monospaced
getFontSize :: OutputHandles -> IO FontSize
getFontSize outs =
    do
        size <-Font.size (font outs) " "
        let size' = (fromIntegral (fst size), fromIntegral (snd size))
        return size'

getWindowSize :: OutputHandles -> IO (Int, Int)
getWindowSize outs = do
    (V2 w h) <- SDL.get (SDL.windowSize (window outs))
    return (fromIntegral w, fromIntegral h)

initOutputHandles :: TextureCfg -> GameConfigs -> IO OutputHandles
initOutputHandles textCfgs cfgs = do
    fontPath <- getGameFullPath fontFile
    SDL.initialize []
    Font.initialize
    window <- SDL.createWindow "My Game" SDL.defaultWindow
        { SDL.windowInitialSize = V2 screenWidth screenHeight
        , SDL.windowHighDPI = True
        , SDL.windowMode = SDL.Windowed
        , SDL.windowResizable = True
        }
    when fs $ SDL.setWindowMode window SDL.FullscreenDesktop
    SDL.showWindow window
    r <- SDL.createRenderer window (-1) rendererConfig
    -- clears the screen
    initWindow r
    font <- Font.load fontPath fontSz
    b <- Font.isMonospace font
    textList <- mapM (loadTexture r) $ M.toList $ getTextureFiles textCfgs
    let textures = M.fromList textList
    print $ fst <$> M.toList textures
    return $ OutputHandles window r textures font
    where
        gCfgs = settingCfgs cfgs
        fontSz = fontSize gCfgs
        fs = fullScreen gCfgs
        screenWidth = fromIntegral $ windowSizeX gCfgs
        screenHeight = fromIntegral $ windowSizeY gCfgs

loadTexture :: SDL.Renderer -> (T.Text, FilePath) -> IO (T.Text, SDL.Texture)
loadTexture r (name, filePath) = do
    path <- getGameFullPath filePath
    t <- SDL.Image.loadTexture r path
    return (name, t)

cleanupOutputHandles :: OutputHandles -> IO ()
cleanupOutputHandles outs = do
    Font.free $ font outs
    Font.quit
    SDL.destroyRenderer $ renderer outs
    SDL.destroyWindow $ window outs
    SDL.quit


executeDraw :: (MonadIO m, OutputRead m, ConfigsRead m) => ToRender -> m ()
executeDraw = drawAll
