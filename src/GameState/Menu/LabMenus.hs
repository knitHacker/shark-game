{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.LabMenus
    ( topLabMenu
    , openResearchMenu
    , completedResearchMenu
    , investigateResearchMenu
    , completedResearchReviewMenu
    ) where

import qualified Data.Text as T
import SaveData
import Configs
import GameState.Types

import OutputHandles.Types

import Shark.Types
import Shark.Research

import Util

topLabMenu :: GameData -> GameConfigs -> Menu
topLabMenu gd cfgs = mkMenu words [] (selOneOpts 20 90 3 20 opts mc) 0
    where
        mc = CursorRect White
        words = [ TextDisplay "Research Lab" 10 10 10 White
                ]
        opts = [ MenuAction "Open Research" $ OpenResearchMenu gd
               , MenuAction "Completed Research" $ CompletedResearchMenu gd
               , MenuAction "Return to Research Center" $ ResearchCenter gd
               ]


openResearchMenu :: GameData -> GameConfigs -> Menu
openResearchMenu gd cfgs = mkMenu words' [] (selOneOpts 20 90 3 20 (opts ++ otherOpts) mc) 0
    where
        sCfgs = sharkCfgs cfgs
        availResearch = getKnownResearch sCfgs gd
        sharksInfo = sharks sCfgs
        mc = CursorRect White
        words = [ TextDisplay "Open Research" 10 10 6 White
                -- , TextDisplay "Discovered Sharks" 15 60 5 Green
                ]
        opts = (\s -> MenuAction (getData s researchPaperName) (InvestigateResearchMenu gd s) ) <$> availResearch
        otherOpts = [ MenuAction "Back" $ LabMenuTop gd ]
        words' = if null opts then words ++ [TextDisplay "Find more sharks to come up with research ideas" 20 60 2 Red] else words

completedResearchMenu :: GameData -> GameConfigs -> Menu
completedResearchMenu gd cfgs = mkMenu words' [] (selOneOpts 20 90 3 20 opts mc) 0
    where
        mc = CursorRect White
        words = [ TextDisplay "Completed Research" 10 10 6 White
                ]
        research = (\s -> MenuAction (getData s researchPaperName) (CompletedResearchReviewMenu gd s)) <$> []
        opts = [ MenuAction "Back" $ LabMenuTop gd ]
        words' = if null research then words ++ [TextDisplay "No completed research" 20 60 2 Red] else words

investigateResearchMenu :: GameData -> DataEntry ResearchData -> GameConfigs -> Menu
investigateResearchMenu gd researchEntry cfgs = mkMenu words' [] (selOneOpts 10 180 3 20 [returnOpt] mc) 0
    where
        mc = CursorRect White
        returnOpt = MenuAction "Back" $ OpenResearchMenu gd
        grantText = T.append "Grant awarded when complete: " (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 10 10 5 White
                , TextDisplay grantText 20 40 2 Green
                ]
        words' = words

completedResearchReviewMenu :: GameData -> DataEntry ResearchData -> GameConfigs -> Menu
completedResearchReviewMenu gd researchEntry cfgs = mkMenu words' [] (selOneOpts 10 180 3 20 [returnOpt] mc) 0
    where
        mc = CursorRect White
        returnOpt = MenuAction "Back" $ OpenResearchMenu gd
        grantText = T.append "Grant awarded when complete: " (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 10 10 5 White
                , TextDisplay grantText 20 40 2 Green
                , TextDisplay (getData researchEntry researchDescription) 20 60 2 White
                ]
        words' = words