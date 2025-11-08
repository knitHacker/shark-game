module Graphics.Animation
    ( updateAnimation
    , startAnimation
    ) where

import Graphics.Types
import InputState

updateAnimation :: AnimationData -> AnimationData
updateAnimation (AnimationData frames maxFrame) = AnimationData (mod (frames + 1) maxFrame) maxFrame

startAnimation :: Int -> AnimationData
startAnimation = AnimationData 0
