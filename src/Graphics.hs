
module Graphics
    ( initGraphics
    , updateGraphics
    ) where

import qualified Data.Map.Strict as M

import Graphics.Types
import Graphics.Menu
import OutputHandles.Types
import OutputHandles
import Configs

-- Assumes monospaced font

initGraphics :: TextureFileMap -> OutputHandles -> IO Graphics
initGraphics tm outs = do
    fontSize <- getFontSize outs
    return Graphics
        { graphicsTextures = M.map (\(TextureCfg x y _) -> TextureInfo x y) tm
        , graphicsFontSize = fontSize
        }

updateGraphics :: Graphics -> Graphics
updateGraphics graphics = graphics
