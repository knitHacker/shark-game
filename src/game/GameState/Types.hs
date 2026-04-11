{-# LANGUAGE ExistentialQuantification #-}

module GameState.Types
    ( AnyGamePlayState(..)
    , GameState(..)
    , GameStateRead(..)
    , GameStateInit(..)
    , GameView(..)
    , OverlayView(..)
    , GameMenu(..)
    , GamePlayState(..)
    , GameStateStep(..)
    , Step(..)
    , Update(..)
    , anyThink
    , anyDraw
    , anyAnimate
    , anyHandleInput
    ) where

import qualified Data.Text as T

import OutputHandles.Types
import Graphics.Types
import Graphics ( GraphicsRead )
import SaveData
import Configs
import InputState


data GameState = GameState
    { gameCurrentState :: !AnyGamePlayState
    , gameLastDraw     :: !(Maybe ToRender)
    }


data GameView = GameView
    { viewLayer   :: View AnyGamePlayState
    , viewOverlay :: Maybe (OverlayView AnyGamePlayState)
    , viewMenu    :: Maybe (Menu AnyGamePlayState)
    }

data OverlayView a = OverlayView
    { isActive    :: Bool
    , overlayView :: View a
    , overlayMenu :: OverlayMenu a
    }

-- Convenience wrapper used by simple menu-only states
data GameMenu = GameMenu
    { gameViewLayer :: View AnyGamePlayState
    , gameMenuLayer :: Menu AnyGamePlayState
    }


-- ---------------------------------------------------------------------------
-- Step and Update: the think/update split

-- | Result after executeAction; consumed by stepGame
data Step
    = UseCache                        -- no state change, preserve gameLastDraw
    | UpdateTo AnyGamePlayState       -- state updated in place, needs re-render
    | TransitionTo AnyGamePlayState   -- transition to a new state, needs re-render

-- | Capability request produced by think; consumed by executeAction
-- Exit is handled by the loop before executeAction
data Update
    = PureStep Step
    | Exit
    | GenerateNewGame (GameData -> Step)
    | SaveFile GameData Step
    | LoadFile FilePath (Either T.Text GameData -> Step)
    | SaveList ([FilePath] -> Step)


-- ---------------------------------------------------------------------------
-- GamePlayState typeclass

class GamePlayState a where
    -- | Called every frame; describes any IO needed and how to advance state
    think :: a -> GameConfigs -> InputState -> Graphics -> Update

    -- | Pure render; called after think resolves and on every window resize
    draw :: a -> Graphics -> GameConfigs -> GameView
    draw _ _ _ = GameView (View [] [] [] [] Nothing) Nothing Nothing

    -- | Per-frame animation step; returns updated state
    animate :: a -> Graphics -> InputState -> a
    animate s _ _ = s

    -- | Per-frame input handling; Left = updated state, Right = transition
    handleInput :: a -> InputState -> Either a AnyGamePlayState
    handleInput s _ = Left s

    {-# MINIMAL think #-}


-- ---------------------------------------------------------------------------
-- Existential wrapper

data AnyGamePlayState = forall a. GamePlayState a => AnyGamePlayState
    { gameState :: a
    , canPause  :: Bool
    }

anyThink :: AnyGamePlayState -> GameConfigs -> InputState -> Graphics -> Update
anyThink (AnyGamePlayState s _) cfgs inputs gr = think s cfgs inputs gr

anyDraw :: AnyGamePlayState -> Graphics -> GameConfigs -> GameView
anyDraw (AnyGamePlayState s _) gr cfgs = draw s gr cfgs

anyAnimate :: AnyGamePlayState -> Graphics -> InputState -> AnyGamePlayState
anyAnimate (AnyGamePlayState s p) gr inputs = AnyGamePlayState (animate s gr inputs) p

anyHandleInput :: AnyGamePlayState -> InputState -> Either AnyGamePlayState AnyGamePlayState
anyHandleInput (AnyGamePlayState s p) inputs =
    case handleInput s inputs of
        Left  s'   -> Left  (AnyGamePlayState s' p)
        Right next -> Right next


-- ---------------------------------------------------------------------------
-- GameStateStep: controls think/update execution
-- Separate instances: AppEnv (real IO) vs PreviewEnv (skip transitions)

class (Monad m, ConfigsRead m, GraphicsRead m, InputRead m) => GameStateStep m where
    -- | Dispatch think; reads input/graphics/configs from the monad
    getUpdate :: GameState -> m Update

    -- | Execute the IO described by Update; return resulting Step
    executeAction :: Update -> m Step

    -- | Apply a Step to produce the next GameState
    -- Nothing = use cached render (no state change)
    -- Just gs = new state to render
    stepGame :: GameState -> Step -> m (Maybe GameState)


-- ---------------------------------------------------------------------------
-- GameStateInit

class Monad m => GameStateInit m where
    loadInitialState :: m GameState


-- ---------------------------------------------------------------------------
-- Reader class (kept for compatibility)

class Monad m => GameStateRead m where
    readGameState :: m GameState
