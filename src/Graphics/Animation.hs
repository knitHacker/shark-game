module Graphics.Animation
    ( updateAnimation
    , startAnimation
    ) where

import Graphics.Types
import InputState

updateAnimation :: AnimationData a -> (AnimationData a, View a)
updateAnimation (AnimationData frame maxFrame action) = (AnimationData newFrame maxFrame action, action newFrame)
    where
        newFrame = mod (frame + 1) maxFrame

startAnimation :: Int -> (Int -> View a) -> AnimationData a
startAnimation = AnimationData 0
