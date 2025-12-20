{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Text as T

import Util
import OutputHandles.Types
import OutputHandles.Util
import Configs
import Shark.Types
import Shark.Review
import GameState.Types
import SaveData
import Graphics.Types
import Graphics.Menu
import Graphics.TextUtil

import Debug.Trace

topReviewMenu :: GameData -> GameConfigs -> GameMenu
topReviewMenu gd cfgs = GameMenu (textView words) (Menu (selOneOpts 250 300 3 20 opts (Just backOpt) mc 0) Nothing)
    where
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 50 50 10 White Nothing ]
        opts = [ MenuAction "Sharks" $ Just $ SharkReviewTop gd Nothing
               , MenuAction "Research" $ Just $ ResearchReviewTop gd
               ]
        backOpt = MenuAction "Return to Research Center" $ Just $ ResearchCenter gd

topReviewSharksMenu :: GameData -> Maybe T.Text -> GameConfigs -> GameMenu
topReviewSharksMenu gd mPrev cfgs = GameMenu (textView words) (Menu options Nothing)
    where
        sharksInfo = sharks $ sharkCfgs cfgs
        mc = CursorRect White
        sharkKeys = zip (M.keys (gameDataFoundSharks gd)) [0..]
        words = [ TextDisplay "Data Review" 50 50 10 White Nothing
                , TextDisplay "Discovered Sharks" 150 200 5 Green Nothing
                ]
        opts = (\(s, _) -> MenuAction (sharkName (sharksInfo M.! s)) $ Just (SharkReview gd (getEntry sharksInfo s))) <$> sharkKeys
        backOpt = MenuAction "Back" $ Just $ DataReviewTop gd
        cursorIdx = case mPrev of
            Nothing -> 0
            Just p -> fromMaybe 0 $ L.lookup p sharkKeys
        options = scrollOpts 200 300 3 20 (BasicSOALOpts (OALOpts opts Nothing mc)) (Just backOpt) [] 4 cursorIdx

sharkReviewMenu :: GameData -> DataEntryT SharkInfo -> GameConfigs -> Graphics -> GameMenu
sharkReviewMenu gd sharkEntry cfgs gr = GameMenu (View ((,0) <$> (locWords ++ words')) imgs [] [] Nothing) (Menu (selOneOpts 500 650 3 20 [] (Just returnOpt) mc 0) Nothing)
    where
        img = getData sharkEntry sharkImage
        imgs = [IPlace 750 200 1.75 img 1]
        locsMap = getSeenLocations (sharkCfgs cfgs) gd sharkEntry
        sharkFinds = getFinds (sharkCfgs cfgs) gd sharkEntry
        mc = CursorRect White
        infoCnts = getInfoCounts sharkFinds
        returnOpt = MenuAction "Back" $ Just $ SharkReviewTop gd (Just (entryKey sharkEntry))
        sightingText = T.append "Shark interactions: " $ T.pack $ show $ length sharkFinds
        countTxts = M.mapWithKey (\i c -> T.concat ["Sharks ", i, " ", T.pack (show c)]) infoCnts
        -- Format locations grouped by region
        (locWords, end) = foldl formatRegion ([], 200) $ M.toList locsMap
        formatRegion (displays, yPos) (region, sites) =
            let regionHeader = TextDisplay region 75 yPos 3 Blue Nothing
                sitesText = T.intercalate ", " sites
                (siteDisplays, newEnd) = wrapText gr sitesText 100 (fromIntegral yPos + 50) 525 3 2 Green
            in (displays ++ [regionHeader] ++ siteDisplays, fromIntegral newEnd + 20)
        words = [ TextDisplay (getData sharkEntry sharkName) 50 50 5 White Nothing
                , TextDisplay "Locations Seen:" 65 140 4 Gray Nothing
                , TextDisplay sightingText 75 (fromIntegral end + 20) 3 White Nothing
                ]
        (_, words') = foldl (\(y, l) t -> (y + 75, l ++ [TextDisplay t 125 y 3 White Nothing])) (fromIntegral end + 75, words) countTxts

topLabMenu :: GameData -> GameConfigs -> GameMenu
topLabMenu gd cfgs = GameMenu (textView words) (Menu (selOneOpts 300 400 4 25 opts (Just backOpt) mc 0) Nothing)
    where
        mc = CursorRect White
        words = [ TextDisplay "Research" 50 20 10 White Nothing
                , TextDisplay "Lab" 200 160 8 White Nothing
                ]
        opts = [ MenuAction "Open Research" $ Just $ OpenResearchMenu gd
               , MenuAction "Completed Research" $ Just $ CompletedResearchMenu gd
               ]
        backOpt = MenuAction "Back" $ Just $ DataReviewTop gd

openResearchMenu :: GameData -> GameConfigs -> GameMenu
openResearchMenu gd cfgs = GameMenu (textView words') (Menu options Nothing)
    where
        sCfgs = sharkCfgs cfgs
        availResearch = filter (\r -> M.notMember (entryKey r) (gameDataResearchComplete gd)) (getKnownResearch sCfgs gd)
        sharksInfo = sharks sCfgs
        mc = CursorRect White
        words = [ TextDisplay "Open Research" 50 50 6 White Nothing
                ]
        opts = (\s -> MenuAction (getData s researchPaperName) $ Just (InvestigateResearchMenu gd s)) <$> availResearch
        backOpt = MenuAction "Back" $ Just $ ResearchReviewTop gd
        options = scrollOpts 150 250 3 25 (BasicSOALOpts (OALOpts opts Nothing mc)) (Just backOpt) [] 4 0
        words' = if null opts then words ++ [TextDisplay "Find more sharks to come up with research ideas" 20 100 2 Red Nothing] else words

completedResearchMenu :: GameData -> GameConfigs -> GameMenu
completedResearchMenu gd cfgs = GameMenu (textView words') (Menu options Nothing)
    where
        mc = CursorRect White
        availResearch = filter (\r -> M.member (entryKey r) (gameDataResearchComplete gd)) $ getKnownResearch (sharkCfgs cfgs) gd
        words = [ TextDisplay "Completed Research" 50 50 6 White Nothing
                ]
        research = (\s -> MenuAction (getData s researchPaperName) $ Just (CompletedResearchReviewMenu gd s)) <$> availResearch
        opts = (\s -> MenuAction (getData s researchPaperName) $ Just (CompletedResearchReviewMenu gd s)) <$> availResearch
        backOpt = MenuAction "Back" $ Just $ ResearchReviewTop gd
        options = scrollOpts 150 250 3 25 (BasicSOALOpts (OALOpts opts Nothing mc)) (Just backOpt) [] 4 0
        words' = if null research then words ++ [TextDisplay "No completed research" 20 100 2 Red Nothing] else words

investigateResearchMenu :: GameData -> DataEntryT ResearchData -> GameConfigs -> Graphics -> GameMenu
investigateResearchMenu gd researchEntry cfgs gr = GameMenu (textView words') (Menu (selOneOpts 200 500 3 20 [completeOpt] (Just returnOpt) mc 0) Nothing)
    where
        reqs = getResearchRequirements (sharkCfgs cfgs) gd researchEntry
        mc = CursorRect White
        gd' = completeResearch (sharkCfgs cfgs) gd researchEntry reqs
        completeOpt = MenuAction "Complete Research" (if canCompleteResearch reqs then Just (AwardGrantMenu gd' researchEntry) else Nothing)
        returnOpt = MenuAction "Back" $ Just $ OpenResearchMenu gd
        grantText = T.append "Grant awarded when complete: $" (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 50 30 5 White Nothing
                , TextDisplay grantText 100 200 2 Green Nothing
                ]
        words' = fst $ foldl makeReqTxt (words, 250) reqs
        makeReqTxt (l, y) (sN, rrs) = (l ++ [sharkHeader] ++ infoRqs, y'' + 75)
                where
                        sharkHeader = TextDisplay sN 150 y 2 White Nothing
                        toText it gsd c = T.concat [T.toTitle it, " ", T.pack (show (length gsd)), "/", T.pack (show c)]
                        (infoRqs, y'') = foldl (\(l', y') (it, gsd, c) -> (l' ++ [TextDisplay (toText it gsd c) 200 y' 2 White Nothing], y' + 30)) ([], y + 50) rrs

awardGrantMenu :: GameData -> DataEntryT ResearchData -> GameConfigs -> Graphics -> GameMenu
awardGrantMenu gd researchEntry cfgs gr = GameMenu (textView words') (Menu (selOneOpts 500 (e + 50) 3 20 [] (Just returnOpt) mc 0) Nothing)
    where
        mc = CursorRect White
        returnOpt = MenuAction "Continue" $ Just $ ResearchReviewTop gd
        grantText = T.append "Grant awarded when complete: $" (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 40 40 5 White Nothing
                , TextDisplay grantText 175 150 3 Green Nothing
                ]
        (descr, e) = wrapText gr (getData researchEntry researchDescription) 100 225 1050 4 2 White
        words' = words ++ descr

completedResearchReviewMenu :: GameData -> DataEntryT ResearchData -> GameConfigs -> Graphics -> GameMenu
completedResearchReviewMenu gd researchEntry cfgs gr = GameMenu (textView words') (Menu (selOneOpts 500 (e + 50) 3 20 [] (Just returnOpt) mc 0) Nothing)
    where
        mc = CursorRect White
        returnOpt = MenuAction "Back" $ Just $ CompletedResearchMenu gd
        grantText = T.append "Grant awarded when completed: $" (T.pack (show (getData researchEntry researchGrant)))
        words = [ TextDisplay (getData researchEntry researchPaperName) 40 40 5 White Nothing
                , TextDisplay grantText 175 150 3 Green Nothing
                ]
        (descr, e) = wrapText gr (getData researchEntry researchDescription) 100 225 1050 4 2 White
        words' = words ++ descr
