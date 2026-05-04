module GameState.Util
    (shouldAnimationStep
    , getAnimationStep
    ) where

import InputState
import Graphics.NewTypes
import GameState.Types

shouldAnimationStep :: InputState -> AnimationState -> Bool
shouldAnimationStep inputs as@(AnimState lastTs interval _ _) = timestamp inputs - lastTs > fromIntegral interval

getAnimationStep :: InputState -> [AnimationState] -> GameStep
getAnimationStep inputs animStates = if not shouldStep then NoChange else StepAnimation ts updatedAnims
    where
        ts = timestamp inputs
        (updatedAnims, shouldStep) = foldl foldFn ([], False) animStates
        foldFn (anims, should) animState
            | shouldAnimationStep inputs animState = (anims ++ [animState { lastAnimTS = ts }], True)
            | otherwise = (anims ++ [animState], should)
