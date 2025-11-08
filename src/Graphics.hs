
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
import InputState

-- Assumes monospaced font

initGraphics :: TextureFileMap -> OutputHandles -> IO Graphics
initGraphics tm outs = do
    fontSize <- getFontSize outs
    return Graphics
        { graphicsTextures = M.map (\(TextureCfg x y _) -> TextureInfo x y) tm
        , graphicsFontSize = fontSize
        }
-- Eventually update potential graphics and font sizing based on config changes
updateGraphics :: Graphics -> GameConfigs -> OutputHandles -> Graphics
updateGraphics graphics _ _ = graphics
