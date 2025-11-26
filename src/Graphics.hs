
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

initGraphics :: TextureCfg -> OutputHandles -> IO Graphics
initGraphics tm outs = do
    fontSize <- getFontSize outs
    let iTxts = M.map (\(ImageTexture sX sY _) -> ImageCfg $ ImageInfo sX sY) $ textureImages tm
        aTxts = M.map (\(AnimationTexture sX sY _ f d) -> AnimationCfg $ AnimationInfo sX sY f d) $ textureAnimations tm
    return Graphics
        { graphicsTextures = M.union iTxts aTxts
        , graphicsFontSize = fontSize
        }
-- Eventually update potential graphics and font sizing based on config changes
updateGraphics :: Graphics -> GameConfigs -> OutputHandles -> Graphics
updateGraphics graphics _ _ = graphics
