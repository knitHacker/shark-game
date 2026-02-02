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
                    updateAnim ap@(APlace x y scale animTexture animFrame animDepth d True) =
                        let anim = graphicsAnimTextures gr ! animTexture
                            mxFrame = animFrameCount anim
                        in ap { animFrame = (animFrame + 1) `mod` mxFrame }
                    updateAnim ap = ap
                in view { animations = updatedAnims }

startAnimation :: Int -> (View a -> Int -> View a) -> AnimationData a
startAnimation toLen f = AnimationData 0 toLen $ GeneralAnimAction f

startTextAnim :: Graphics -> AnimationData a
startTextAnim gr = AnimationData 0 maxBound TextureAnimAction

useUpdate :: AnimPlacement -> AnimPlacement -> AnimPlacement
useUpdate apPos (APlace _ _ _ _ animFrame animDepth _ show) = apPos { animFrame = animFrame, animDepth = animDepth, animShow = show }