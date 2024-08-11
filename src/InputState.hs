module InputState
    ( InputState(..)
    , initInputState
    , InputRead(..)
    , updateInput
    , Direction(..)
    , escapePressed
    , escapeJustPressed
    , enterPressed
    , enterJustPressed
    , iPressed
    , spacePressed
    , moveInputPressed
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


data InputState = InputState
    { inputStateQuit :: Bool
    , inputStateDirection :: Maybe Direction
    , inputControl :: Maybe KeyPress
    , inputRepeat :: Bool
    , inputTimestamp :: Word32
    } deriving (Show, Eq)


initInputState :: IO InputState
initInputState = return $ InputState False Nothing Nothing False 0


class Monad m => InputRead m where
    readInputState :: m InputState

escapePressed :: InputState -> Bool
escapePressed (InputState _ _ (Just EscapePress) _ _) = True
escapePressed _ = False

escapeJustPressed :: InputState -> Bool
escapeJustPressed (InputState _ _ (Just EscapePress) False _) = True
escapeJustPressed _ = False

enterPressed :: InputState -> Bool
enterPressed (InputState _ _ (Just EnterPress) _ _) = True
enterPressed _ = False

enterJustPressed :: InputState -> Bool
enterJustPressed (InputState _ _ (Just EnterPress) False _) = True
enterJustPressed _ = False

iPressed :: InputState -> Bool
iPressed (InputState _ _ (Just IPress) _ _) = True
iPressed _ = False

spacePressed :: InputState -> Bool
spacePressed (InputState _ _ (Just SpacePress) _ _) = True
spacePressed _ = False

moveInputPressed :: InputState -> Bool
moveInputPressed (InputState _ (Just _) _ _ _) = True
moveInputPressed _ = False


updateInput :: (InputRead m, MonadIO m) => m InputState
updateInput = do
    input <- readInputState
    time <- liftIO getSystemTime
    event <- SDL.pollEvent
    let ts = div (systemNanoseconds time) 1000000
    case event of
        (Just event) -> return $ payloadToIntent event ts
        _ -> return $ input { inputRepeat = True, inputTimestamp = ts }


payloadToIntent :: SDL.Event -> Word32 -> InputState
payloadToIntent (SDL.Event _ SDL.QuitEvent) ts = InputState True Nothing Nothing False ts
payloadToIntent (SDL.Event _ (SDL.KeyboardEvent k)) ts =
    case getKey k of
        Nothing -> InputState False Nothing Nothing False ts
        Just (r, Left ctr) -> InputState False Nothing (Just ctr) r ts
        Just (r, Right d) -> InputState False (Just d) Nothing r ts
payloadToIntent (SDL.Event _ _) ts = InputState False Nothing Nothing False ts


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
