{-# LANGUAGE Strict #-}

module GameState
    ( initGameState
    , updateGameState
    ) where

import InputState
import GameState.Types
import GameState.Menu.GameMenus (initialMainMenuState)
import Graphics (GraphicsRead(..))
import SaveData (GameData)

initGameState :: Maybe GameData -> GameState
initGameState gdM = GameState (initialMainMenuState gdM) Nothing


updateGameState
    :: (GameStateRead m, GameStateStep m, InputRead m, GraphicsRead m)
    => m (Maybe GameState)
updateGameState = do
    gs     <- readGameState
    inputs <- readInputState
    gr     <- readGraphics
    let state  = gameCurrentState gs
        state' = anyAnimate state gr inputs
    case anyHandleInput state' inputs of
        Left  state'' -> return $ Just $ GameState state'' Nothing
        Right next    -> do
            step   <- executeThink next
            mState <- resolveStep next step
            return $ fmap (\s -> GameState s Nothing) mState
