{-# LANGUAGE OverloadedStrings #-}

-- TODO: port to GamePlayState typeclass (step 3)

module GameState.Menu.LabMenus
    ( labTopMenu
    , fleetManagementTopMenu
    , equipmentManagementTopMenu
    , equipmentStoreMenu
    , fundraiserTopMenu
    , boatStoreMenu
    , chooseActiveBoatMenu
    , donorList
    , fundraisingMenu
    ) where

import qualified Data.Text as T

import Configs
import SaveData
import GameState.Types
import Graphics.Types
import InputState

labTopMenu :: GameData -> Graphics -> GameMenu
labTopMenu _ _ = error "TODO: labTopMenu not yet ported to typeclass"

fleetManagementTopMenu :: GameData -> GameConfigs -> InputState -> Graphics -> GameView
fleetManagementTopMenu _ _ _ _ = error "TODO: fleetManagementTopMenu not yet ported to typeclass"

equipmentManagementTopMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
equipmentManagementTopMenu _ _ _ = error "TODO: equipmentManagementTopMenu not yet ported to typeclass"

equipmentStoreMenu :: Maybe (T.Text, Int, GameData) -> GameData -> GameConfigs -> Graphics -> GameMenu
equipmentStoreMenu _ _ _ _ = error "TODO: equipmentStoreMenu not yet ported to typeclass"

fundraiserTopMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
fundraiserTopMenu _ _ _ = error "TODO: fundraiserTopMenu not yet ported to typeclass"

boatStoreMenu :: Maybe (T.Text, Int, GameData) -> GameData -> GameConfigs -> Graphics -> GameMenu
boatStoreMenu _ _ _ _ = error "TODO: boatStoreMenu not yet ported to typeclass"

chooseActiveBoatMenu :: GameData -> GameConfigs -> GameMenu
chooseActiveBoatMenu _ _ = error "TODO: chooseActiveBoatMenu not yet ported to typeclass"

donorList :: GameData -> Graphics -> GameMenu
donorList _ _ = error "TODO: donorList not yet ported to typeclass"

fundraisingMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
fundraisingMenu _ _ _ = error "TODO: fundraisingMenu not yet ported to typeclass"
