module Graphics.Animation
    ( updateAnimation
    , startAnimation
    ) where

import Graphics.Types
import InputState

updateAnimation :: AnimationData a -> (AnimationData a, View a)
updateAnimation ad@(AnimationData frame maxFrame action) = (ad { animationFrame = newFrame }, action newFrame)
    where
        newFrame = mod (frame + 1) maxFrame

startAnimation :: Int -> (Int -> View a) -> AnimationData a
startAnimation = AnimationData 0

updateAnimationTexture :: AnimationData a -> AnimationInfo -> (AnimationData a, View a)
updateAnimationTexture (AnimationData frame maxFrame action) animInfo = undefined
    where
        newFrame = mod (frame + 1) (animFrameCount animInfo)
        newAd = AnimationData newFrame (animFrameCount animInfo) action
