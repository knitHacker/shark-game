{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.DataReviewMenu
    ( topReviewMenu
    , topReviewSharksMenu
    , sharkReviewMenu
    , topLabMenu
    , openResearchMenu
    , completedResearchMenu
    , investigateResearchMenu
    , completedResearchReviewMenu
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Util
import OutputHandles.Types
import Configs
import Shark.Types
import Shark.Review
import GameState.Types
import SaveData

topReviewMenu :: GameData -> GameConfigs -> Menu
topReviewMenu gd cfgs = mkMenu words [] (selOneOpts 20 60 3 20 opts mc) 0
    where
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 10 10 10 White
                ]
        opts = [ MenuAction "Sharks" $ SharkReviewTop gd
               , MenuAction "Research" $ ResearchReviewTop gd
               , MenuAction "Return to Research Center" $ ResearchCenter gd
               ]


topReviewSharksMenu :: GameData -> GameConfigs -> Menu
topReviewSharksMenu gd cfgs = mkMenu words [] (selOneOpts 20 90 3 20 (opts ++ otherOpts) mc) 0
    where
        sharksInfo = sharks $ sharkCfgs cfgs
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 10 10 10 White
                , TextDisplay "Discovered Sharks" 15 60 5 Green
                ]
        opts = (\s -> MenuAction (sharkName (sharksInfo M.! s)) (SharkReview gd (getEntry sharksInfo s))) <$> (M.keys (gameDataFoundSharks gd))
        otherOpts = [ MenuAction "Return to Research Center" $ ResearchCenter gd ]

sharkReviewMenu :: GameData -> DataEntry SharkInfo -> GameConfigs -> Menu
sharkReviewMenu gd sharkEntry cfgs = mkMenu words' [] (selOneOpts 30 180 3 20 [returnOpt] mc) 0
    where
        sharkFinds = getFinds (sharkCfgs cfgs) gd sharkEntry
        mc = CursorRect White
        infoCnts = getInfoCounts sharkFinds
        returnOpt = MenuAction "Back" $ DataReviewTop gd
        sightingText = T.append "Shark interactions: " $ T.pack $ show $ length sharkFinds
        countTxts = M.mapWithKey (\i c -> T.concat ["Sharks ", i, " ", T.pack (show c)]) infoCnts
        words = [ TextDisplay (getData sharkEntry sharkName) 10 10 6 White
                , TextDisplay sightingText 20 40 4 White
                ]
        (_, words') = foldl (\(y, l) t -> (y + 20, l ++ [TextDisplay t 30 y 3 White])) (80, words) countTxts


topLabMenu :: GameData -> GameConfigs -> Menu
topLabMenu gd cfgs = mkMenu words [] (selOneOpts 20 90 3 20 opts mc) 0
    where
        mc = CursorRect White
        words = [ TextDisplay "Research Lab" 10 10 10 White
                ]
        opts = [ MenuAction "Open Research" $ OpenResearchMenu gd
               , MenuAction "Completed Research" $ CompletedResearchMenu gd
               , MenuAction "Return to Research Center" $ DataReviewTop gd
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
        otherOpts = [ MenuAction "Back" $ ResearchReviewTop gd ]
        words' = if null opts then words ++ [TextDisplay "Find more sharks to come up with research ideas" 20 60 2 Red] else words

completedResearchMenu :: GameData -> GameConfigs -> Menu
completedResearchMenu gd cfgs = mkMenu words' [] (selOneOpts 20 90 3 20 opts mc) 0
    where
        mc = CursorRect White
        words = [ TextDisplay "Completed Research" 10 10 6 White
                ]
        research = (\s -> MenuAction (getData s researchPaperName) (CompletedResearchReviewMenu gd s)) <$> []
        opts = [ MenuAction "Back" $ ResearchReviewTop gd ]
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
