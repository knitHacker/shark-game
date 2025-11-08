{-# LANGUAGE Strict #-}

module InputState
    ( InputState(..)
    , KeyboardInputs(..)
    , MouseInputs(..)
    , InputRead(..)
    , Direction(..)
    , inputRepeating
    , initInputState
    , updateInput
    , inputDirection
    , escapePressed
    , escapeJustPressed
    , enterPressed
    , enterJustPressed
    , iPressed
    , spacePressed
    , moveInputPressed
    , inputQuit
    ) where

import qualified SDL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Time.Clock.System
    ( SystemTime(..), getSystemTime )

import Data.Word (Word32)

import Debug.Trace

data Direction
    = DUp
    | DDown
    | DLeft
    | DRight
    deriving (Show, Eq)

data KeyPress
    = EnterPress
    | EscapePress
    | IPress
    | SpacePress
    deriving (Show, Eq)


data InputState = InputState
    { keyInputs :: !(Maybe KeyboardInputs)
    , mouseInputs :: !(Maybe MouseInputs)
    , timestamp :: !Int64
    }

data MouseInputs = MouseInputs
    { scrollAmt :: !Int
    }

data KeyboardInputs = Keyboard
    { inputStateQuit :: !Bool
    , inputStateDirection :: !(Maybe Direction)
    , inputControl :: !(Maybe KeyPress)
    , inputRepeat :: !Bool
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState Nothing Nothing 0


class Monad m => InputRead m where
    readInputState :: m InputState

escapePressed :: InputState -> Bool
escapePressed (InputState (Just (Keyboard _ _ (Just EscapePress) _)) _ _) = True
escapePressed _ = False

escapeJustPressed :: InputState -> Bool
escapeJustPressed (InputState (Just (Keyboard _ _ (Just EscapePress) False)) _ _) = True
escapeJustPressed _ = False

enterPressed :: InputState -> Bool
enterPressed (InputState (Just (Keyboard _ _ (Just EnterPress) _)) _ _) = True
enterPressed _ = False

enterJustPressed :: InputState -> Bool
enterJustPressed (InputState (Just (Keyboard _ _ (Just EnterPress) False)) _ _) = True
enterJustPressed _ = False

iPressed :: InputState -> Bool
iPressed (InputState (Just (Keyboard _ _ (Just IPress) _)) _ _) = True
iPressed _ = False

spacePressed :: InputState -> Bool
spacePressed (InputState (Just (Keyboard _ _ (Just SpacePress) _)) _ _) = True
spacePressed _ = False

moveInputPressed :: InputState -> Bool
moveInputPressed (InputState (Just (Keyboard _ (Just _) _ _)) _ _) = True
moveInputPressed _ = False


updateRepeat :: InputState -> Int64 -> InputState
updateRepeat (InputState (Just (Keyboard sq sd c _)) _ _) ts = InputState (Just (Keyboard sq sd c True)) Nothing ts
updateRepeat (InputState Nothing _ _) ts = InputState Nothing Nothing ts

inputRepeating :: InputState -> Bool
inputRepeating (InputState (Just (Keyboard _ _ _ r)) _ _) = r
inputRepeating _ = False

inputDirection :: InputState -> Maybe Direction
inputDirection (InputState (Just key) _ _) = inputStateDirection key
inputDirection _ = Nothing

inputQuit :: InputState -> Bool
inputQuit (InputState (Just key) _ _) = inputStateQuit key
inputQuit _ = False

updateInput :: (InputRead m, MonadIO m) => Word32 -> m InputState
updateInput to = do
    input <- readInputState
    time <- liftIO getSystemTime
    event <- SDL.waitEventTimeout (fromIntegral to)
    let ts = systemSeconds time
        tn = systemNanoseconds time
        tsInt = ts * 1000 + fromIntegral (div tn 1000000)
    case event of
        (Just event) -> do
            return $ payloadToIntent event tsInt
        _ -> return $ updateRepeat input tsInt


payloadToIntent :: SDL.Event -> Int64 -> InputState
payloadToIntent (SDL.Event _ SDL.QuitEvent) ts = InputState (Just (Keyboard True Nothing Nothing False)) Nothing ts
payloadToIntent (SDL.Event _ (SDL.KeyboardEvent k)) ts =
    case getKey k of
        Nothing -> InputState Nothing Nothing ts
        Just (r, Left ctr) -> InputState (Just (Keyboard False Nothing (Just ctr) r)) Nothing ts
        Just (r, Right d) -> InputState (Just (Keyboard False (Just d) Nothing r)) Nothing ts
payloadToIntent (SDL.Event _ (SDL.MouseWheelEvent mwd)) ts = InputState Nothing (Just (MouseInputs (getScroll mwd))) ts
payloadToIntent (SDL.Event _ _) ts = InputState Nothing Nothing ts


getScroll :: SDL.MouseWheelEventData -> Int
getScroll (SDL.MouseWheelEventData _ _ (SDL.V2 _ y) _) = fromIntegral y

getKey :: SDL.KeyboardEventData -> Maybe (Bool, Either KeyPress Direction)
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed repeat keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeUp     -> Just (repeat, Right DUp)
    SDL.KeycodeDown   -> Just (repeat, Right DDown)
    SDL.KeycodeLeft   -> Just (repeat, Right DLeft)
    SDL.KeycodeRight  -> Just (repeat, Right DRight)
    SDL.KeycodeReturn -> Just (repeat, Left EnterPress)
    SDL.KeycodeEscape -> Just (repeat, Left EscapePress)
    SDL.KeycodeSpace  -> Just (repeat, Left SpacePress)
    SDL.KeycodeI      -> Just (repeat, Left IPress)
    _                 -> Nothing
