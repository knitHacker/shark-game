{-# LANGUAGE ExistentialQuantification #-}

module GameState.Types
    ( AnyGamePlayState(..)
    , GameState(..)
    , GameStateRead(..)
    , GameView(..)
    , OverlayView(..)
    , GameMenu(..)
    , GamePlayState(..)
    , GameStateStep(..)
    , Step(..)
    , Update(..)
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


-- ---------------------------------------------------------------------------
-- Core game state.
-- No view stored here — draw is called on demand by the render layer.
-- Each state value carries its own display state (cursor position, anim frames, scroll offset).

data GameState = GameState
    { gameCurrentState :: !AnyGamePlayState
    , gameCurrentView :: !GameView
    , gameLastDraw     :: !(Maybe ToRender)
    }


-- ---------------------------------------------------------------------------
-- View types
-- GameView is the return type of draw — a snapshot of what to render right now.
-- It is never stored between frames.

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

-- | What to do after executing an Update's IO
data Step
    = Animate
    | HandleInput
    | AnimateAndHandleInput
    | UseCache -- no change
    | Resize
    | Transition AnyGamePlayState

-- | IO action a state needs when first entered
data Update
    = JustStep Step
    | GenerateNewGame (GameData -> Step)
    | SaveFile GameData Step -- should probably deal with making sure saving actually happened but ignore for now
    | LoadFile FilePath (Either T.Text GameData -> Step)
    | SaveList ([FilePath] -> Step)


-- ---------------------------------------------------------------------------
-- GamePlayState typeclass
--
-- Each old sum-type constructor becomes a data type with an instance here.
-- animate and handleInput return an updated `a` so the state owns all
-- mutable display state (cursor position, animation frames, scroll offset).
-- draw is then a pure function of the state — no merging needed.
--
-- States with no animation or generic-only input can rely on the defaults.
-- States needing custom behaviour (e.g. TripProgress, animated backgrounds)
-- override animate / handleInput and can import helpers from GameState.Helpers.

class GamePlayState a where
    -- | Called once on state entry; describes any IO needed before first draw
    think :: a -> GameConfigs -> Update

    -- | Pure render; called after think resolves and on every window resize
    -- i don't think i need this since it shouldn't be dependent on the class since it has a common resulting render type
--    draw :: a -> Graphics -> GameConfigs -> GameView

    -- | Per-frame animation step; returns updated state.
    -- Default: state returned unchanged (no animation).
    animate :: a -> Graphics -> InputState -> a
    animate s _ _ = s

    -- | Per-frame input handling; returns updated state or transition.
    -- Left = updated state (cursor moved, scroll, etc.)
    -- Right = transition to a new state
    -- Default: state returned unchanged (no input handling).
    handleInput :: a -> InputState -> a
    handleInput s _ = Left s

    {-# MINIMAL think #-}


-- ---------------------------------------------------------------------------
-- Existential wrapper

data AnyGamePlayState = forall a. GamePlayState a => AnyGamePlayState
    { gameState :: a
    , canPause  :: Bool
    }

-- Dispatch class methods through the existential

anyDraw :: AnyGamePlayState -> Graphics -> GameConfigs -> GameView
anyDraw (AnyGamePlayState s _) gr cfgs = draw s gr cfgs

-- Returns a new AnyGamePlayState with the updated inner state
anyAnimate :: AnyGamePlayState -> Graphics -> InputState -> AnyGamePlayState
anyAnimate (AnyGamePlayState s p) gr inputs = AnyGamePlayState (animate s gr inputs) p

-- Left = updated state (cursor moved, scroll, etc.); Right = transition out
anyHandleInput :: AnyGamePlayState -> InputState -> Either AnyGamePlayState AnyGamePlayState
anyHandleInput (AnyGamePlayState s p) inputs =
    case handleInput s inputs of
        Left  s'   -> Left  (AnyGamePlayState s' p)
        Right next -> Right next


-- ---------------------------------------------------------------------------
-- GameStateStep: controls think/update execution
-- Separate instances: AppEnv (real IO) vs PreviewEnv (skip transitions)

class (Monad m, ConfigsRead m, GraphicsRead m) => GameStateStep m where
    -- | Execute the IO described by Update; return resulting Step
    executeThink :: AnyGamePlayState -> m Step

    -- | Apply a Step; recurses through Transitions until stable
    -- Nothing = Exit
    resolveStep :: AnyGamePlayState -> Step -> m (Maybe AnyGamePlayState)


-- ---------------------------------------------------------------------------
-- Reader class

class Monad m => GameStateRead m where
    readGameState :: m GameState
