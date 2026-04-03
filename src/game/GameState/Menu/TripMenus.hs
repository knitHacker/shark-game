{-# LANGUAGE OverloadedStrings #-}

-- TODO: port to GamePlayState typeclass (step 3)

module GameState.Menu.TripMenus
    ( mapMenu
    , equipmentPickMenu
    , reviewTripMenu
    , tripProgressMenu
    , sharkFoundMenu
    , tripResultsMenu
    ) where

import qualified Data.Text as T

import Configs
import SaveData
import GameState.Types
import Graphics.Types
import InputState
import Shark.Types
import Shark.Trip

mapMenu :: GameData -> Int -> Graphics -> GameConfigs -> GameMenu
mapMenu _ _ _ _ = error "TODO: mapMenu not yet ported to typeclass"

equipmentPickMenu :: GameData -> (Int, T.Text) -> [T.Text] -> Int -> GameConfigs -> GameMenu
equipmentPickMenu _ _ _ _ _ = error "TODO: equipmentPickMenu not yet ported to typeclass"

reviewTripMenu :: GameData -> (Int, T.Text) -> [T.Text] -> GameConfigs -> GameMenu
reviewTripMenu _ _ _ _ = error "TODO: reviewTripMenu not yet ported to typeclass"

tripProgressMenu :: GameData -> TripState -> GameConfigs -> InputState -> Graphics -> GameView
tripProgressMenu _ _ _ _ _ = error "TODO: tripProgressMenu not yet ported to typeclass"

sharkFoundMenu :: GameData -> Maybe SharkFind -> TripState -> GameConfigs -> Graphics -> GameMenu
sharkFoundMenu _ _ _ _ _ = error "TODO: sharkFoundMenu not yet ported to typeclass"

tripResultsMenu :: GameData -> TripState -> GameConfigs -> Graphics -> GameMenu
tripResultsMenu _ _ _ _ = error "TODO: tripResultsMenu not yet ported to typeclass"
