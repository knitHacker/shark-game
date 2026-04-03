{-# LANGUAGE OverloadedStrings #-}

-- TODO: port to GamePlayState typeclass (step 3)

module GameState.Menu.DataReviewMenu
    ( topReviewMenu
    , topReviewSharksMenu
    , sharkReviewMenu
    , topLabMenu
    , openResearchMenu
    , completedResearchMenu
    , investigateResearchMenu
    , completedResearchReviewMenu
    , awardGrantMenu
    ) where

import qualified Data.Text as T

import Configs
import SaveData
import GameState.Types
import Graphics.Types
import Shark.Types
import Shark.Review
import Util

topReviewMenu :: GameData -> GameConfigs -> GameMenu
topReviewMenu _ _ = error "TODO: topReviewMenu not yet ported to typeclass"

topReviewSharksMenu :: GameData -> Maybe T.Text -> GameConfigs -> Graphics -> GameMenu
topReviewSharksMenu _ _ _ _ = error "TODO: topReviewSharksMenu not yet ported to typeclass"

sharkReviewMenu :: GameData -> DataEntryT SharkInfo -> GameConfigs -> Graphics -> GameMenu
sharkReviewMenu _ _ _ _ = error "TODO: sharkReviewMenu not yet ported to typeclass"

topLabMenu :: GameData -> GameConfigs -> GameMenu
topLabMenu _ _ = error "TODO: topLabMenu not yet ported to typeclass"

openResearchMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
openResearchMenu _ _ _ = error "TODO: openResearchMenu not yet ported to typeclass"

completedResearchMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
completedResearchMenu _ _ _ = error "TODO: completedResearchMenu not yet ported to typeclass"

investigateResearchMenu :: GameData -> DataEntryT ResearchData -> GameConfigs -> Graphics -> GameMenu
investigateResearchMenu _ _ _ _ = error "TODO: investigateResearchMenu not yet ported to typeclass"

completedResearchReviewMenu :: GameData -> DataEntryT ResearchData -> GameConfigs -> Graphics -> GameMenu
completedResearchReviewMenu _ _ _ _ = error "TODO: completedResearchReviewMenu not yet ported to typeclass"

awardGrantMenu :: GameData -> DataEntryT ResearchData -> GameConfigs -> Graphics -> GameMenu
awardGrantMenu _ _ _ _ = error "TODO: awardGrantMenu not yet ported to typeclass"
