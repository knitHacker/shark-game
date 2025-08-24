{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
module OutputHandles
    ( initOutputHandles
    , cleanupOutputHandles
    , executeDraw
    , getFontSize
    ) where


import Foreign.C.Types ()
import qualified SDL
import qualified SDL.Image
import SDL.Vect ( V2(V2) )
import SDL                    (($=))
import qualified SDL.Font as Font
import Control.Monad ()
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
        let rX = ratioX outs
            rY = ratioY outs
            size' = (fromIntegral (fst size) / rX, fromIntegral (snd size) / rY)
        print size'
        return size'

initOutputHandles :: TextureFileMap -> GameConfigs -> IO OutputHandles
initOutputHandles textCfgs cfgs = do
    fontPath <- getGameFullPath fontFile
    SDL.initialize []
    Font.initialize
    window <- SDL.createWindow "My Game" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window
    r <- SDL.createRenderer window (-1) rendererConfig
    -- clears the screen
    initWindow r
    font <- Font.load fontPath 16
    b <- Font.isMonospace font
    textList <- mapM (loadTexture r) $ M.toList textCfgs
    let textures = M.fromList textList
    print $ fst <$> M.toList textures
    return $ OutputHandles window r textures font ratioX ratioY
    where
        gCfgs = settingCfgs cfgs
        screenWidth = fromIntegral $ windowSizeX gCfgs
        screenHeight = fromIntegral $ windowSizeY gCfgs
        boardX = fromIntegral $ boardSizeX gCfgs
        boardY = fromIntegral $ boardSizeY gCfgs
        ratioX = fromIntegral screenWidth / fromIntegral boardX
        ratioY = fromIntegral screenHeight / fromIntegral boardY

loadTexture :: SDL.Renderer -> (T.Text, TextureCfg) -> IO (T.Text, SDL.Texture)
loadTexture r (name, textureCfg) = do
    path <- getGameFullPath $ file textureCfg
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
