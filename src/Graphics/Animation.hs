module Graphics.Animation
    ( updateAnimation
    , startAnimation
    , startTextAnim
    , useUpdate
    ) where

import Graphics.Types
import InputState
import Data.IntMap (update)
import Data.Map.Strict ((!))

updateAnimation :: Graphics -> View a -> AnimationData a -> (AnimationData a, View a)
updateAnimation gr view ad@(AnimationData frame maxFrame action) = (ad { animationFrame = newFrame }, newView)
    where
        newFrame = mod (frame + 1) maxFrame
        newView = case action of
            GeneralAnimAction f -> f view newFrame
            TextureAnimAction ->
                let anims = animations view
                    updatedAnims = map updateAnim anims
                    updateAnim ap@(APlace x y scale animTexture animFrame animDepth d) =
                        let anim = graphicsAnimTextures gr ! animTexture
                            mxFrame = animFrameCount anim
                            mxDepth = animFrameDepth anim
                        in ap { animFrame = newFrame `mod` mxFrame, animDepth = animDepth `mod` mxDepth }
                in view { animations = updatedAnims }

startAnimation :: Int -> (View a -> Int -> View a) -> AnimationData a
startAnimation toLen f = AnimationData 0 toLen $ GeneralAnimAction f

startTextAnim :: Graphics -> [AnimPlacement] -> AnimationData a
startTextAnim _ [] = AnimationData 0 1 TextureAnimAction
startTextAnim gr apL = AnimationData 0 maxFrame TextureAnimAction
    where
        maxFrame = maximum $ map getMaxFrame apL
        getMaxFrame (APlace _ _ _ animTexture _ _ _) = animFrameCount (graphicsAnimTextures gr ! animTexture)


useUpdate :: AnimPlacement -> AnimPlacement -> AnimPlacement
useUpdate apPos (APlace _ _ _ _ animFrame animDepth _) = apPos { animFrame = animFrame, animDepth = animDepth }