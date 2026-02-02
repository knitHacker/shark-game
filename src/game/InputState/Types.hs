{-# LANGUAGE Strict #-}

module InputState.Types
    ( InputState(..)
    , KeyboardInputs(..)
    , MouseInputs(..)
    , Direction(..)
    , KeyPress(..)
    ) where

import Data.Int (Int64)

data Direction
    = DUp
    | DDown
    | DLeft
    | DRight
    deriving (Show, Eq)

data InputState = InputState
    { keyInputs :: !(Maybe KeyboardInputs)
    , mouseInputs :: !(Maybe MouseInputs)
    , windowResized :: !(Maybe (Int, Int))
    , timestamp :: !Int64
    }

data MouseInputs = MouseInputs
    { scrollAmt :: !Int
    }

data KeyPress
    = EnterPress
    | EscapePress
    | BackPress
    | IPress
    | SpacePress
    deriving (Show, Eq)

data KeyboardInputs = Keyboard
    { inputStateQuit :: !Bool
    , inputStateDirection :: !(Maybe Direction)
    , inputControl :: !(Maybe KeyPress)
    , inputRepeat :: !Bool
    } deriving (Show, Eq)
