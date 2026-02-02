{-# LANGUAGE Strict #-}

module InputState
    ( InputState(..)
    , KeyboardInputs(..)
    , MouseInputs(..)
    , Direction(..)
    , inputRepeating
    , initInputState
    , updateInput
    , inputDirection
    , escapePressed
    , escapeJustPressed
    , enterPressed
    , enterJustPressed
    , backPressed
    , backJustPressed
    , iPressed
    , spacePressed
    , moveInputPressed
    , inputQuit
    , wasWindowResized
    ) where

import qualified SDL
import qualified SDL.Input.GameController as SDL.GameController
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64, Int16)
import Data.Time.Clock.System
    ( SystemTime(..), getSystemTime )
import Capabilities (InputRead(..), TimeRead(..))
import InputState.Types

import Data.Word (Word32)

import Debug.Trace

initInputState :: (MonadIO m, TimeRead m) => m InputState
initInputState = do
    time <- getCurrentTime
    let ts = systemSeconds time
        tn = systemNanoseconds time
        tsInt = ts * 1000 + fromIntegral (div tn 1000000)
    return $ InputState Nothing Nothing Nothing tsInt

escapePressed :: InputState -> Bool
escapePressed (InputState (Just (Keyboard _ _ (Just EscapePress) _)) _ _ _) = True
escapePressed _ = False

escapeJustPressed :: InputState -> Bool
escapeJustPressed (InputState (Just (Keyboard _ _ (Just EscapePress) False)) _ _ _) = True
escapeJustPressed _ = False

enterPressed :: InputState -> Bool
enterPressed (InputState (Just (Keyboard _ _ (Just EnterPress) _)) _ _ _) = True
enterPressed _ = False

enterJustPressed :: InputState -> Bool
enterJustPressed (InputState (Just (Keyboard _ _ (Just EnterPress) False)) _ _ _) = True
enterJustPressed _ = False

backPressed :: InputState -> Bool
backPressed (InputState (Just (Keyboard _ _ (Just BackPress) _)) _ _ _) = True
backPressed _ = False

backJustPressed :: InputState -> Bool
backJustPressed (InputState (Just (Keyboard _ _ (Just BackPress) False)) _ _ _) = True
backJustPressed _ = False

iPressed :: InputState -> Bool
iPressed (InputState (Just (Keyboard _ _ (Just IPress) _)) _ _ _) = True
iPressed _ = False

spacePressed :: InputState -> Bool
spacePressed (InputState (Just (Keyboard _ _ (Just SpacePress) _)) _ _ _) = True
spacePressed _ = False

moveInputPressed :: InputState -> Bool
moveInputPressed (InputState (Just (Keyboard _ (Just _) _ _)) _ _ _) = True
moveInputPressed _ = False


updateRepeat :: InputState -> Int64 -> InputState
updateRepeat (InputState (Just (Keyboard sq sd c _)) _ _ _) ts = InputState (Just (Keyboard sq sd c True)) Nothing Nothing ts
updateRepeat (InputState Nothing _ _ _) ts = InputState Nothing Nothing Nothing ts

inputRepeating :: InputState -> Bool
inputRepeating (InputState (Just (Keyboard _ _ _ r)) _ _ _) = r
inputRepeating _ = False

wasWindowResized :: InputState -> Bool
wasWindowResized (InputState _ _ (Just _) _) = True
wasWindowResized _ = False

inputDirection :: InputState -> Maybe Direction
inputDirection (InputState (Just key) _ _ _) = inputStateDirection key
inputDirection _ = Nothing

inputQuit :: InputState -> Bool
inputQuit (InputState (Just key) _ _ _) = inputStateQuit key
inputQuit _ = False

updateInput :: (InputRead m, TimeRead m, MonadIO m) => Word32 -> m InputState
updateInput to = do
    input <- readInputState
    time <- getCurrentTime
    event <- SDL.waitEventTimeout (fromIntegral to)
    let ts = systemSeconds time
        tn = systemNanoseconds time
        tsInt = ts * 1000 + fromIntegral (div tn 1000000)
    case event of
        (Just event) -> do
            return $ payloadToIntent event tsInt
        _ -> return $ updateRepeat input tsInt


payloadToIntent :: SDL.Event -> Int64 -> InputState
payloadToIntent (SDL.Event _ SDL.QuitEvent) ts = InputState (Just (Keyboard True Nothing Nothing False)) Nothing Nothing ts
payloadToIntent (SDL.Event _ (SDL.KeyboardEvent k)) ts =
    case getKey k of
        Nothing -> InputState Nothing Nothing Nothing ts
        Just (r, Left ctr) -> InputState (Just (Keyboard False Nothing (Just ctr) r)) Nothing Nothing ts
        Just (r, Right d) -> InputState (Just (Keyboard False (Just d) Nothing r)) Nothing Nothing ts
payloadToIntent (SDL.Event _ (SDL.MouseWheelEvent mwd)) ts = InputState Nothing (Just (MouseInputs (getScroll mwd))) Nothing ts
payloadToIntent (SDL.Event _ (SDL.WindowResizedEvent (SDL.WindowResizedEventData _ (SDL.V2 w h)))) ts = InputState Nothing Nothing (Just (fromIntegral w, fromIntegral h)) ts
payloadToIntent (SDL.Event _ (SDL.ControllerButtonEvent cbd)) ts =
    case getControllerButton cbd of
        Nothing -> InputState Nothing Nothing Nothing ts
        Just (r, Left ctr) -> InputState (Just (Keyboard False Nothing (Just ctr) r)) Nothing Nothing ts
        Just (r, Right d) -> InputState (Just (Keyboard False (Just d) Nothing r)) Nothing Nothing ts
payloadToIntent (SDL.Event _ (SDL.ControllerAxisEvent cad)) ts =
    case getControllerAxis cad of
        Nothing -> InputState Nothing Nothing Nothing ts
        Just d -> InputState (Just (Keyboard False (Just d) Nothing False)) Nothing Nothing ts
payloadToIntent (SDL.Event _ _) ts = InputState Nothing Nothing Nothing ts


getScroll :: SDL.MouseWheelEventData -> Int
getScroll (SDL.MouseWheelEventData _ _ (SDL.V2 _ y) _) = fromIntegral y

getKey :: SDL.KeyboardEventData -> Maybe (Bool, Either KeyPress Direction)
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Nothing
getKey (SDL.KeyboardEventData _ SDL.Pressed repeat keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeUp        -> Just (repeat, Right DUp)
    SDL.KeycodeDown      -> Just (repeat, Right DDown)
    SDL.KeycodeLeft      -> Just (repeat, Right DLeft)
    SDL.KeycodeRight     -> Just (repeat, Right DRight)
    SDL.KeycodeReturn    -> Just (repeat, Left EnterPress)
    SDL.KeycodeEscape    -> Just (repeat, Left EscapePress)
    SDL.KeycodeBackspace -> Just (repeat, Left BackPress)
    SDL.KeycodeSpace     -> Just (repeat, Left SpacePress)
    SDL.KeycodeI         -> Just (repeat, Left IPress)
    _                    -> Nothing

getControllerButton :: SDL.ControllerButtonEventData -> Maybe (Bool, Either KeyPress Direction)
getControllerButton (SDL.ControllerButtonEventData _ button SDL.GameController.ControllerButtonReleased) = Nothing
getControllerButton (SDL.ControllerButtonEventData _ button SDL.GameController.ControllerButtonPressed) =
  case button of
    SDL.GameController.ControllerButtonDpadUp       -> Just (False, Right DUp)
    SDL.GameController.ControllerButtonDpadDown     -> Just (False, Right DDown)
    SDL.GameController.ControllerButtonDpadLeft     -> Just (False, Right DLeft)
    SDL.GameController.ControllerButtonDpadRight    -> Just (False, Right DRight)
    SDL.GameController.ControllerButtonA            -> Just (False, Left EnterPress)
    SDL.GameController.ControllerButtonB            -> Just (False, Left BackPress)
    SDL.GameController.ControllerButtonStart        -> Just (False, Left EnterPress)
    SDL.GameController.ControllerButtonBack         -> Just (False, Left EscapePress)
    SDL.GameController.ControllerButtonX            -> Just (False, Left SpacePress)
    SDL.GameController.ControllerButtonY            -> Just (False, Left IPress)
    _                                               -> Nothing
getControllerButton _ = Nothing

getControllerAxis :: SDL.ControllerAxisEventData -> Maybe Direction
getControllerAxis (SDL.ControllerAxisEventData _ axis value) =
  case axis of
    SDL.GameController.ControllerAxisLeftX
      | value > 16384   -> Just DRight
      | value < -16384  -> Just DLeft
      | otherwise       -> Nothing
    SDL.GameController.ControllerAxisLeftY
      | value > 16384   -> Just DDown
      | value < -16384  -> Just DUp
      | otherwise       -> Nothing
    _                   -> Nothing
