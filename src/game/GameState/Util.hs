module GameState.Util
    (shouldAnimationStep
    , getAnimationStep
    , pauseOverlay
    , PauseOpts(..)
    ) where

import InputState
import Graphics.NewTypes
import Graphics.Types
import GameState.Types
import Graphics.TextUtil
import OutputHandles.Types

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

data PauseOpts = ContinuePause | MainMenuPause | ExitPause
               deriving (Show, Ord, Bounded, Enum, Eq)

pauseOverlay :: Graphics -> Overlay
pauseOverlay gr = AOverlay assets (Just overMenu) (ox gr) (oy gr) ow oh Blue $ Just oRsz
    where
        ow = 750
        oh = 600
        ox gr' = midStartX gr' ow
        oy gr' = midStartY gr' oh
        assets = undefined
        mRsz mAss gr' = mAss { menuXBase = ox gr' + 150, menuYBase = oy gr' + 300 }
        overMenu = MenuAsset ((ox gr) + 150) ((oy gr) + 300) 2 (Just mRsz) 3 mItems
        oRsz ov gr' = ov { oAssets = resizeAssets (oAssets ov) gr', oMenu = resizeMenu (oMenu ov) gr', overlayX = ox gr', overlayY = oy gr' }
        mItem = [ MenuItem "Continue" White
                , MenuItem "Main Menu" White
                , MenuItem "Save & Exit" White
                ]
