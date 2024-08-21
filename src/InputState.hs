{-# LANGUAGE Strict #-}
module InputState
    ( InputState(..)
    , KeyboardInputs(..)
    , inputRepeating
    , initInputState
    , InputRead(..)
    , updateInput
    , Direction(..)
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
import Data.Word (Word32)
import Data.Time.Clock.System
    ( SystemTime(systemNanoseconds), getSystemTime )

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


newtype InputState = InputState (Maybe KeyboardInputs)

data KeyboardInputs = Keyboard
    { inputStateQuit :: !Bool
    , inputStateDirection :: !(Maybe Direction)
    , inputControl :: !(Maybe KeyPress)
    , inputRepeat :: !Bool
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState Nothing


class Monad m => InputRead m where
    readInputState :: m InputState

escapePressed :: InputState -> Bool
escapePressed (InputState (Just (Keyboard _ _ (Just EscapePress) _))) = True
escapePressed _ = False

escapeJustPressed :: InputState -> Bool
escapeJustPressed (InputState (Just (Keyboard _ _ (Just EscapePress) False))) = True
escapeJustPressed _ = False

enterPressed :: InputState -> Bool
enterPressed (InputState (Just (Keyboard _ _ (Just EnterPress) _))) = True
enterPressed _ = False

enterJustPressed :: InputState -> Bool
enterJustPressed (InputState (Just (Keyboard _ _ (Just EnterPress) False))) = True
enterJustPressed _ = False

iPressed :: InputState -> Bool
iPressed (InputState (Just (Keyboard _ _ (Just IPress) _))) = True
iPressed _ = False

spacePressed :: InputState -> Bool
spacePressed (InputState (Just (Keyboard _ _ (Just SpacePress) _))) = True
spacePressed _ = False

moveInputPressed :: InputState -> Bool
moveInputPressed (InputState (Just (Keyboard _ (Just _) _ _))) = True
moveInputPressed _ = False


updateRepeat :: InputState -> InputState
updateRepeat (InputState (Just (Keyboard sq sd c _))) = InputState (Just (Keyboard sq sd c True))
updateRepeat (InputState Nothing) = InputState Nothing

inputRepeating :: InputState -> Bool
inputRepeating (InputState (Just (Keyboard _ _ _ r))) = r
inputRepeating _ = False

inputDirection :: InputState -> Maybe Direction
inputDirection (InputState (Just key)) = inputStateDirection key
inputDirection _ = Nothing

inputQuit :: InputState -> Bool
inputQuit (InputState (Just key)) = inputStateQuit key
inputQuit _ = False

updateInput :: (InputRead m, MonadIO m) => m InputState
updateInput = do
    input <- readInputState
    event <- SDL.pollEvent
    case event of
        (Just event) -> return $ payloadToIntent event
        _ -> return $ updateRepeat input


payloadToIntent :: SDL.Event -> InputState
payloadToIntent (SDL.Event _ SDL.QuitEvent) = InputState (Just (Keyboard True Nothing Nothing False))
payloadToIntent (SDL.Event _ (SDL.KeyboardEvent k)) =
    case getKey k of
        Nothing -> InputState Nothing
        Just (r, Left ctr) -> InputState (Just (Keyboard False Nothing (Just ctr) r))
        Just (r, Right d) -> InputState (Just (Keyboard False (Just d) Nothing r))
payloadToIntent (SDL.Event _ _) = InputState Nothing


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
