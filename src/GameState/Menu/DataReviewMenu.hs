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
    , awardGrantMenu
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Util
import OutputHandles.Types
import OutputHandles.Util
import Configs
import Shark.Types
import Shark.Review
import GameState.Types
import SaveData

import Debug.Trace

topReviewMenu :: GameData -> GameConfigs -> Menu
topReviewMenu gd cfgs = mkMenu words [] (selOneOpts 20 60 3 20 opts mc) 0
    where
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 10 10 10 White
                ]
        opts = [ MenuAction "Sharks" True $ SharkReviewTop gd
               , MenuAction "Research" True $ ResearchReviewTop gd
               , MenuAction "Return to Research Center" True $ ResearchCenter gd
               ]


topReviewSharksMenu :: GameData -> GameConfigs -> Menu
topReviewSharksMenu gd cfgs = mkMenu words [] (selOneOpts 20 90 3 20 (opts ++ otherOpts) mc) 0
    where
        sharksInfo = sharks $ sharkCfgs cfgs
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 10 10 10 White
                , TextDisplay "Discovered Sharks" 15 60 5 Green
                ]
        opts = (\s -> MenuAction (sharkName (sharksInfo M.! s)) True (SharkReview gd (getEntry sharksInfo s))) <$> (M.keys (gameDataFoundSharks gd))
        otherOpts = [ MenuAction "Back" True $ DataReviewTop gd ]

sharkReviewMenu :: GameData -> DataEntry SharkInfo -> GameConfigs -> Menu
sharkReviewMenu gd sharkEntry cfgs = mkMenu words' [] (selOneOpts 30 180 3 20 [returnOpt] mc) 0
    where
        sharkFinds = getFinds (sharkCfgs cfgs) gd sharkEntry
        mc = CursorRect White
        infoCnts = getInfoCounts sharkFinds
        returnOpt = MenuAction "Back" True $ SharkReviewTop gd
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
        opts = [ MenuAction "Open Research" True $ OpenResearchMenu gd
               , MenuAction "Completed Research" True $ CompletedResearchMenu gd
               , MenuAction "Back" True $ DataReviewTop gd
               ]


openResearchMenu :: GameData -> GameConfigs -> Menu
openResearchMenu gd cfgs = mkMenu words' [] (selOneOpts 20 90 3 20 (opts ++ otherOpts) mc) 0
    where
        sCfgs = sharkCfgs cfgs
        availResearch = filter (\r -> M.notMember (entryKey r) (gameDataResearchComplete gd)) (getKnownResearch sCfgs gd)
        sharksInfo = sharks sCfgs
        mc = CursorRect White
        words = [ TextDisplay "Open Research" 10 10 6 White
                ]
        opts = (\s -> MenuAction (getData s researchPaperName) True (InvestigateResearchMenu gd s) ) <$> availResearch
        otherOpts = [ MenuAction "Back" True $ ResearchReviewTop gd ]
        words' = if null opts then words ++ [TextDisplay "Find more sharks to come up with research ideas" 20 60 2 Red] else words

completedResearchMenu :: GameData -> GameConfigs -> Menu
completedResearchMenu gd cfgs = mkMenu words' [] (selOneOpts 20 90 3 20 (opts ++ otherOpts) mc) 0
    where
        mc = CursorRect White
        availResearch = filter (\r -> M.member (entryKey r) (gameDataResearchComplete gd)) $ getKnownResearch (sharkCfgs cfgs) gd
        words = [ TextDisplay "Completed Research" 10 10 6 White
                ]
        research = (\s -> MenuAction (getData s researchPaperName) True (CompletedResearchReviewMenu gd s)) <$> availResearch
        opts = (\s -> MenuAction (getData s researchPaperName) True (CompletedResearchReviewMenu gd s) ) <$> availResearch
        otherOpts = [ MenuAction "Back" True $ ResearchReviewTop gd ]
        words' = if null research then words ++ [TextDisplay "No completed research" 20 60 2 Red] else words

investigateResearchMenu :: GameData -> DataEntry ResearchData -> GameConfigs -> Menu
investigateResearchMenu gd researchEntry cfgs = mkMenu words' [] (selOneOpts 10 180 3 20 [completeOpt, returnOpt] mc) 0
    where
        reqs = getResearchRequirements (sharkCfgs cfgs) gd researchEntry
        mc = CursorRect White
        gd' = completeResearch (sharkCfgs cfgs) gd researchEntry reqs
        completeOpt = MenuAction "Complete Research" (canCompleteResearch reqs) $ AwardGrantMenu gd' researchEntry
        returnOpt = MenuAction "Back" True $ OpenResearchMenu gd
        grantText = T.append "Grant awarded when complete: " (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 10 10 5 White
                , TextDisplay grantText 20 40 2 Green
                ]
        words' = fst $ foldl makeReqTxt (words, 60) reqs
        makeReqTxt (l, y) (sN, rrs) = (l ++ [sharkHeader] ++ infoRqs, y'' + 20)
                where
                        sharkHeader = TextDisplay sN 25 y 3 White
                        (infoRqs, y'') = foldl (\(l', y') (it, gsd, c) -> (l' ++ [TextDisplay (T.concat [T.toTitle it, " ", T.pack (show (length gsd)), "/", T.pack (show c)]) 30 y' 2 White], y' + 10)) ([], y + 20) rrs

awardGrantMenu :: GameData -> DataEntry ResearchData -> GameConfigs -> OutputHandles -> Menu
awardGrantMenu gd researchEntry cfgs outs = mkMenu words' [] (selOneOpts 10 180 3 20 [returnOpt] mc) 0
    where
        mc = CursorRect White
        returnOpt = MenuAction "Continue" True $ ResearchReviewTop gd
        grantText = T.append "Grant awarded when complete: " (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 10 10 5 White
                , TextDisplay grantText 20 40 2 Green
                ]
        (descr, _) = wrapText outs (getData researchEntry researchDescription) 20 60 120 4 3 White
        words' = words ++ descr

completedResearchReviewMenu :: GameData -> DataEntry ResearchData -> GameConfigs -> OutputHandles -> Menu
completedResearchReviewMenu gd researchEntry cfgs outs = mkMenu words' [] (selOneOpts 10 180 3 20 [returnOpt] mc) 0
    where
        mc = CursorRect White
        returnOpt = MenuAction "Back" True $ CompletedResearchMenu gd
        grantText = T.append "Grant awarded when completed: " (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 10 10 5 White
                , TextDisplay grantText 20 40 3 Green
                ]
        (descr, _) = wrapText outs (getData researchEntry researchDescription) 20 60 200 4 2 White
        words' = words ++ descr