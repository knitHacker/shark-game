
module Graphics
    ( initGraphics
    , updateGraphics
    ) where

import qualified Data.Map.Strict as M

import Graphics.Types
import Graphics.Menu
import Draw.Types
import Configs
import InputState

import Handles.Types
import Handles

-- Assumes monospaced font

initGraphics :: TextureCfg -> Handles -> IO Graphics
initGraphics tm outs = do
    fontSize <- getFontSize outs
    (width, height) <- getWindowSize outs
    let iTxts = M.map (\(ImageTexture sX sY _) -> ImageInfo sX sY) $ textureImages tm
        aTxts = M.map (\(AnimationTexture sX sY _ f d) -> AnimationInfo sX sY f d) $ textureAnimations tm
    return Graphics
        { graphicsStaticTextures = iTxts
        , graphicsAnimTextures = aTxts
        , graphicsFontSize = fontSize
        , graphicsWindowWidth = width
        , graphicsWindowHeight = height
        }
-- Eventually update potential graphics and font sizing based on config changes
updateGraphics :: Graphics -> GameConfigs -> InputState -> Graphics
updateGraphics graphics _ inputState =
    case windowResized inputState of
        Nothing -> graphics
        Just (width, height) -> graphics { graphicsWindowWidth = width, graphicsWindowHeight = height }
