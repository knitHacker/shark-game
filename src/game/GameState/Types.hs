{-# LANGUAGE ExistentialQuantification #-}

module GameState.Types
    ( AnyGamePlayState(..)
    , GameOverlay(..)
    , GameState(..)
    , GameView(..)
    , OverlayView(..)
    , GameMenu(..)
    , ViewMenu(..)
    , GamePlayState(..)
    , GameStateStep(..)
    , Step(..)
    , Update(..)
    , anyThink
    , anyDraw
    , anyInitialize
    ) where

import qualified Data.Text as T

import OutputHandles.Types
import Graphics.Types
import Graphics.Types ( GraphicsRead )
import SaveData
import Configs
import InputState

-- Note: Okay only thing that is a little muddled is the GameState. There is confusion between a state transition
-- and state value. This might be a problem partially because gameView holds 2 things, state like cursor position
-- but also text / color / position. Ideally I would like to split them but you want them together when making
-- the render itself and the structure of the type depends on what is in the view.

-- Okay need to get this top level refactor going so I guess keep as close to what I had before.

data GameOverlay = PauseOverlay !Int !GameData

data GameState = GameState
    { gameCurrentState :: !AnyGamePlayState
    , gameView         :: !GameView
    , gameLastDraw     :: !(Maybe ToRender)
    , gameOverlay      :: !(Maybe GameOverlay)
    }


-- | Wraps Menu Update; exists to avoid a GameState.Types -> Graphics.Types circular import
newtype ViewMenu = ViewMenu (Menu Update)

data GameView = GameView
    { viewLayer   :: View AnyGamePlayState
    , viewOverlay :: Maybe (OverlayView Update)
    , viewMenu    :: Maybe ViewMenu
    }

data OverlayView a = OverlayView
    { isActive    :: Bool
    , overlayView :: View a
    , overlayMenu :: OverlayMenu a
    }

-- Convenience wrapper used by simple menu-only states
data GameMenu = GameMenu
    { gameViewLayer :: View AnyGamePlayState
    , gameMenuLayer :: ViewMenu
    }


-- ---------------------------------------------------------------------------
-- Step and Update: the think/update split

-- | Result after executeAction; consumed by stepGame
data Step
    = UseCache                        -- no state change, preserve gameLastDraw
    | Refresh AnyGamePlayState       -- state updated in place, needs re-render
    | Transition AnyGamePlayState   -- transition to a new state, needs re-render
    | SetOverlay (Maybe GameOverlay)  -- Just overlay = open/update; Nothing = close

-- | Capability request produced by think; consumed by executeAction
-- Exit is handled by the loop before executeAction
data Update
    = PureStep Step
    | Exit
    | GenerateNewGame (GameData -> Step)
    | SaveFile GameData Step
    | SaveAndExit GameData
    | LoadFile FilePath (Either T.Text GameData -> Step)
    | SaveList ([FilePath] -> Step)


-- ---------------------------------------------------------------------------
-- GamePlayState typeclass

class GamePlayState a where
    -- | Called every frame; describes any IO needed and how to advance state
    think :: a -> GameConfigs -> InputResult -> Graphics -> Update

    -- | Pure render; called when state changes (Refresh or Transition)
    draw :: a -> Graphics -> GameConfigs -> GameView
    draw _ _ _ = GameView (View [] [] [] [] Nothing) Nothing Nothing

    -- | Build the initial GameState when transitioning into this state
    initialize :: a -> Bool -> Graphics -> GameConfigs -> GameState
    initialize s p gr cfgs = GameState (AnyGamePlayState s p) (draw s gr cfgs) Nothing Nothing

    {-# MINIMAL think #-}


-- ---------------------------------------------------------------------------
-- Existential wrapper

data AnyGamePlayState = forall a. GamePlayState a => AnyGamePlayState
    { gameState :: a
    , canPause  :: Bool
    }

anyThink :: AnyGamePlayState -> GameConfigs -> InputResult -> Graphics -> Update
anyThink (AnyGamePlayState s _) cfgs inputs gr = think s cfgs inputs gr

anyDraw :: AnyGamePlayState -> Graphics -> GameConfigs -> GameView
anyDraw (AnyGamePlayState s _) gr cfgs = draw s gr cfgs

anyInitialize :: AnyGamePlayState -> Graphics -> GameConfigs -> GameState
anyInitialize (AnyGamePlayState s p) gr cfgs = initialize s p gr cfgs


-- ---------------------------------------------------------------------------
-- GameStateStep: controls think/update execution
-- Separate instances: AppEnv (real IO) vs PreviewEnv (skip transitions)

class Monad m => GameStateStep m where
    -- | Dispatch think; reads graphics/configs from the monad, input passed explicitly
    getUpdate :: InputResult -> GameState -> m Update

    -- | Execute the IO described by Update; return resulting Step
    executeAction :: Update -> m (Maybe Step)

    -- | Apply a Step to produce the next GameState
    -- Nothing = use cached render (no state change)
    -- Just gs = new state to render
    stepGame :: GameState -> Step -> m (Maybe GameState)

