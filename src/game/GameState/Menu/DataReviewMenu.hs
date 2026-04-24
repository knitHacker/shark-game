{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.DataReviewMenu
    ( TopReviewState(..)
    , initTopReviewState
    , SharkReviewTopState(..)
    , initSharkReviewTopState
    , SharkReviewState(..)
    , initSharkReviewState
    , ResearchReviewState(..)
    , initResearchReviewState
    , OpenResearchState(..)
    , initOpenResearchState
    , CompletedResearchState(..)
    , initCompletedResearchState
    , SubmitResearchState(..)
    , initSubmitResearchState
    , AwardGrantState(..)
    , initAwardGrantState
    , CompletedResearchReviewState(..)
    , initCompletedResearchReviewState
    ) where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set as S

import Util
import OutputHandles.Types
import OutputHandles.Util
import Configs
import Shark.Types
import Shark.Review
import Shark.Util
import GameState.Types
import GameState.Util
import SaveData
import Graphics.Types
import Graphics.TextUtil
import Graphics.Asset

import InputState

import Debug.Trace

data TopReviewOpt = ReviewSharkOpt | ReviewResearchOpt | ReviewReturnOpt
                  deriving (Show, Eq, Enum, Ord, Bounded)


data TopReviewState = TopReviewState (MenuStateInfo TopReviewOpt)

initTopReviewState :: GameData -> TopReviewState
initTopReviewState gd = TopReviewState $ initMenuStateInfo gd

defaultTopReview :: Graphics -> GView
defaultTopReview gr = GView assets mempty $ Just menu
    where
        assets = M.fromList $ zip [0..]
                                  [ staticText "Data Review" White 50 50 10 0
                                  ]
        menu = DefaultMenu $ MenuAsset 250 300 0 Nothing 20 menuItems
        menuItems = [ MenuItem (MText "Sharks") Blue 3 0 0 True Nothing Nothing
                    , MenuItem (MText "Research") Blue 3 0 0 True Nothing Nothing
                    , MenuItem (MText "Return to Research Center") Blue 3 0 0 True Nothing Nothing
                    ]

applyTopReview :: TopReviewState -> Graphics -> GView -> GView
applyTopReview (TopReviewState msi) gr gv = menuInfoApply msi gv gr id

instance GamePlayStateE TopReviewState where
    think gps@(TopReviewState msi) cfgs inputs = menuInfoThink TopReviewState next msi inputs
        where
            gd = gamedata $ stateInfo msi
            next ReviewSharkOpt = Step $ Transition $ AnyGamePlayState $ initSharkReviewTopState gd
            next ReviewResearchOpt = Step $ Transition $ AnyGamePlayState $ initResearchReviewState gd
            next ReviewReturnOpt = Step $ TopTransition ResearchCenterMenu gd

    transition gps cfgs gr = (applyTopReview gps gr gview, [])
        where
            gview = defaultTopReview gr

    update gps@(TopReviewState msi) gv cfgs gr = menuInfoUpdate TopReviewState msi gv gr id


data SharkReviewTopState = SharkReviewTopState (MenuStateInfo Int)

initSharkReviewTopState :: GameData -> SharkReviewTopState
initSharkReviewTopState gd = SharkReviewTopState $ MenuInfo 0 $ initSimpleStateInfo gd

sharkInfo :: GameData -> GameConfigs -> [DataEntryT SharkInfo]
sharkInfo gd cfgs = getEntry allSharks <$> sharkKeys
    where
        sharkKeys = M.keys $ gameDataFoundSharks gd
        numSharks = length sharkKeys
        allSharks = sharks $ sharkCfgs cfgs


initSharkTopView :: GameData -> GameConfigs -> Graphics -> GView
initSharkTopView gd cfgs gr = gview
    where
        fs = 4
        ls = 4
        gview = GView assets mempty $ Just (ScrollMenu $ sharkScrollResize baseMenu gr)
        assets = M.fromList $ zip [0..]
                                  [ staticText "Data Review" White 50 20 8 0
                                  , staticText "Discovered Sharks" Green 75 150 4 0
                                  ]
        sharkEntries = sharkInfo gd cfgs
        items = (\(Entry _ e) -> ScrollMenuItem (MText (sharkName e)) Blue 0 Nothing Nothing) <$> sharkEntries
        baseMenu = ScrollMenuAsset 100 300 0 (Just sharkScrollResize) fs ls items 0 0
                                    [MenuItem (MText "Back") Blue fs 0 0 True Nothing Nothing]


sharkScrollResize :: ScrollMenuAsset -> Graphics -> ScrollMenuAsset
sharkScrollResize menu gr = menu { menuScrollStartIdx = newStart, menuScrollViewable = newViewable }
    where
        scItems = menuScrollItems menu
        mv = menuScrollViewable menu
        height = (graphicsWindowHeight gr) - (menuScrollY menu) - 100
        entryH = round (fromIntegral (menuFontSize menu) * fontHeight gr) + menuScrollLineSpace menu
        newViewable = height `div` entryH
        cutOff = take (mv - newViewable) (drop (newViewable + menuScrollStartIdx menu) scItems)
        fstHLM = L.findIndex (isJust . highlightedScrollColor) cutOff
        newStart
            | newViewable >= mv = menuScrollStartIdx menu
            | otherwise = case fstHLM of
                            Nothing -> menuScrollStartIdx menu
                            Just idx -> menuScrollStartIdx menu + idx + 1

instance GamePlayStateE SharkReviewTopState where
    think gps@(SharkReviewTopState msi) cfgs inputs = menuInfoIntThink newGPS chooseShark (length sharks) msi inputs
        where
            gd = gamedata $ stateInfo msi
            sharks = sharkInfo gd cfgs
            newGPS = SharkReviewTopState
            chooseShark idx
                | idx == length sharks = Step $ Transition $ AnyGamePlayState $ initTopReviewState gd
                | otherwise = Step $ Transition $ AnyGamePlayState $ initSharkReviewState gd $ sharks L.!! idx

    transition gps@(SharkReviewTopState msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            gview = initSharkTopView gd cfgs gr
            gd = gamedata $ stateInfo msi

    update gps@(SharkReviewTopState msi) gv cfgs gr = menuInfoUpdate SharkReviewTopState msi gv gr id


buildSharkScrollAssets :: Graphics -> GameData -> DataEntryT SharkInfo -> M.Map Int Asset
buildSharkScrollAssets gr gd sharkEntry = M.fromList $ zip [0..] factAssets
    where
        facts    = getFactsFound gd sharkEntry
        lineH sz = ceiling (fontHeight gr * fromIntegral sz) :: Int
        scrollW  = graphicsWindowWidth gr - 150
        (factAssets, _) = foldl buildFact ([], 0) facts
        buildFact (acc, y) fact =
            let titleA  = staticText (sharkFactTitle fact) Yellow 100 y 3 0
                descY   = y + lineH 3 + 5
                descObj = wrapTextStack gr (scrollW - 100) White (sharkFactInfo fact) 2 3
                descA   = Asset descObj 150 descY 0 True Nothing
            in (acc ++ [titleA, descA], descY + assetObjHeight gr descObj + 15)

data SharkReviewState = SharkReviewState
    { srsEntry ::(DataEntryT SharkInfo)
    , srsScPos :: Int
    , srsInfo :: SimpleStateInfo
    }

initSharkReviewState :: GameData -> DataEntryT SharkInfo -> SharkReviewState
initSharkReviewState gd se = SharkReviewState se 0 $ initSimpleStateInfo gd

defaultSharkReview :: SharkReviewState -> GameConfigs -> Graphics -> GView
defaultSharkReview (SharkReviewState sharkEntry _ si) cfgs gr = GView assets (overlays pSelM) $ Just (singleMenu gr "Back" 50 1)
    where
        gd           = gamedata si
        pSelM        = pauseSelM si
        sCfgs        = sharkCfgs cfgs
        imgKey       = getData sharkEntry sharkImage
        locsMap      = getSeenLocations sCfgs gd sharkEntry
        sharkFinds   = getFinds sCfgs gd sharkEntry
        infoCnts     = getInfoCounts sharkFinds
        lineH sz     = ceiling (fontHeight gr * fromIntegral sz) :: Int
        scrollW      = graphicsWindowWidth gr - 150
        sightingText = T.append "Shark interactions: " $ T.pack $ show $ length sharkFinds
        (locAssets, end) = foldl formatRegion ([], 150) $ M.toList locsMap
        formatRegion (acc, yPos) (region, sites) =
            let regionA = staticText region Blue 75 yPos 3 0
                siteY   = yPos + lineH 3 + 5
                siteObj = wrapTextStack gr scrollW Green (T.intercalate ", " sites) 2 3
                siteA   = Asset siteObj 100 siteY 0 True Nothing
            in (acc ++ [regionA, siteA], siteY + assetObjHeight gr siteObj + 20)
        sightA = staticText sightingText White 80 end 2 0
        (yInteract, countAssets) = foldl buildCount (end + lineH 2 + 10, []) (M.toList infoCnts)
        buildCount (y, acc) (infoType, cnt) =
            let txt = T.concat [T.toTitle (equipTypeText infoType), ": ", T.pack (show cnt)]
            in (y + lineH 2 + 10, acc ++ [staticText txt White 130 y 2 0])
        imgScale gr' = min 1.5 (fromIntegral (graphicsWindowWidth gr') / 900.0)
        imgInfo gr'  = graphicsStaticTextures gr' M.! imgKey
        imgW gr'     = floor $ fromIntegral (imageSizeX (imgInfo gr')) * imgScale gr'
        imgX gr'     = graphicsWindowWidth gr' - imgW gr' - 30
        imgRs a gr'  = a { object = AssetImage imgKey (imgScale gr'), assetX = imgX gr' }
        imgA         = Asset (AssetImage imgKey (imgScale gr)) (imgX gr) 200 1 True (Just imgRs)
        scrollYStart = yInteract + 20
        optYStart gr' = graphicsWindowHeight gr' - 70
        scrollH gr'   = optYStart gr' - 5 - scrollYStart
        scrollRs as gr' = as { object = AssetScroll $ case object as of
            AssetScroll sc -> sc { scrollSeenHeight = scrollH gr' }
            _ -> ScrollObj M.empty 0 (scrollH gr') }
        factItems    = buildSharkScrollAssets gr gd sharkEntry
        scrollA      = Asset (AssetScroll $ ScrollObj factItems 0 (scrollH gr)) 50 scrollYStart 0 True (Just scrollRs)
        nameA        = staticText (getData sharkEntry sharkName) White 20 20 5 0
        locHeaderA   = staticText "Locations Seen:" Gray 65 90 4 0
        allStaticAssets = [nameA, locHeaderA] ++ locAssets ++ [sightA] ++ countAssets ++ [imgA]
        assets       = M.fromList $ zip [0..] (allStaticAssets ++ [scrollA])
        overlays Nothing     = mempty
        overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel

applySharkReview :: SharkReviewState -> GView -> GameConfigs -> Graphics -> GView
applySharkReview (SharkReviewState _ scrPos _) gv _ gr = gv { assets = M.map upScroll (assets gv) }
    where
        upScroll a = case object a of
            AssetScroll sc -> a { object = AssetScroll $ updateScrollPosition gr sc scrPos }
            _ -> a


instance GamePlayStateE SharkReviewState where
    think gps@(SharkReviewState sharkEntry scrPos si) cfgs inputs =
        case mouseInputs inputs of
            Nothing -> simpleInfoThink (SharkReviewState sharkEntry scrPos) enterPressed si inputs
            Just (MouseInputs sAmt) -> stepInputUpdate $ SharkReviewState sharkEntry (max 0 (scrPos + 5 * (-sAmt))) si
        where
            gd = gamedata si
            sharkKeys = M.keys $ gameDataFoundSharks gd
            restoredIdx = fromMaybe 0 $ L.elemIndex (entryKey sharkEntry) sharkKeys
            enterPressed = Step $ Transition $ AnyGamePlayState $ SharkReviewTopState $ MenuInfo restoredIdx $ SimpleInfo gd Nothing

    transition gps cfgs gr = (applySharkReview gps (defaultSharkReview gps cfgs gr) cfgs gr, [])

    update gps@(SharkReviewState sharkEntry scrPos si) gv cfgs gr = withPauseUpdate gr newGPS pSelM nGv gv
        where
            pSelM  = pauseSelM si
            maxP   = foldl (\acc a -> case object a of { AssetScroll sc -> max acc (getMaxPosition gr sc); _ -> acc }) 0 (M.elems (assets gv))
            newGPS = SharkReviewState sharkEntry (min scrPos maxP) si
            nGv gv' = applySharkReview newGPS gv' cfgs gr


data ResearchTopOpt = ResearchOpenOpt | ResearchCompleteOpt | ResearchBackOpt
                    deriving (Show, Eq, Ord, Enum, Bounded)

data ResearchReviewState = ResearchReviewState (MenuStateInfo ResearchTopOpt)

initResearchReviewState :: GameData -> ResearchReviewState
initResearchReviewState gd = ResearchReviewState $ initMenuStateInfo gd

instance GamePlayStateE ResearchReviewState where
    think gps@(ResearchReviewState msi) cfgs inputs = menuInfoThink ResearchReviewState next msi inputs
        where
            gd = menuGetData msi
            next ResearchOpenOpt = Step $ Transition $ AnyGamePlayState $ initOpenResearchState gd
            next ResearchCompleteOpt = Step $ Transition $ AnyGamePlayState $ initCompletedResearchState gd
            next ResearchBackOpt = Step $ Transition $ AnyGamePlayState $ initTopReviewState gd

    transition gps@(ResearchReviewState msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            gview = GView assets mempty $ Just menu
            assets = M.fromList $ zip [0..]
                [ staticText "Data Review" White 50 20 8 0
                , staticText "Research" LightGray 75 150 4 0
                ]
            items =
                [ MenuItem (MText "Open Research")      Blue 3 0 0 True Nothing Nothing
                , MenuItem (MText "Completed Research") Blue 3 0 0 True Nothing Nothing
                , MenuItem (MText "Back")               Blue 3 0 0 True Nothing Nothing
                ]
            menu = DefaultMenu $ MenuAsset 140 300 1 Nothing 8 items

-- todo use the menuinfoupdate function
    update gps@(ResearchReviewState msi) gv cfgs gr = menuInfoUpdate ResearchReviewState msi gv gr id


researchScrollResize :: ScrollMenuAsset -> Graphics -> ScrollMenuAsset
researchScrollResize menu gr = menu { menuScrollStartIdx = newStart, menuScrollViewable = newViewable }
    where
        scItems = menuScrollItems menu
        mv = menuScrollViewable menu
        menuYStart = graphicsWindowHeight gr - (ceiling (fontHeight gr * 3) + 50)
        height = menuYStart - menuScrollY menu - 20
        entryH = round (fromIntegral (menuFontSize menu) * fontHeight gr) + menuScrollLineSpace menu
        newViewable = height `div` entryH
        cutOff = take (mv - newViewable) (drop (newViewable + menuScrollStartIdx menu) scItems)
        fstHLM = L.findIndex (isJust . highlightedScrollColor) cutOff
        newStart
            | newViewable >= mv = menuScrollStartIdx menu
            | otherwise = case fstHLM of
                            Nothing -> menuScrollStartIdx menu
                            Just idx -> menuScrollStartIdx menu + idx + 1

researchScrollMenu :: Graphics -> [ScrollMenuItem] -> AssetMenu
researchScrollMenu gr items = ScrollMenu ScrollMenuAsset
    { menuScrollX = 50
    , menuScrollY = scrollYStart
    , menuScrollLayer = 0
    , menuScrollResize = Just researchScrollResize
    , menuScrollLineSpace = lineSpace
    , menuFontSize = fontSize
    , menuScrollItems = items
    , menuScrollStartIdx = 0
    , menuScrollViewable = viewable
    , menuStationaryItems = [MenuItem (MText "Back") Blue fontSize 0 0 True Nothing Nothing]
    }
    where
        fontSize = 3
        lineSpace = 8
        scrollYStart = 240
        menuYStart = graphicsWindowHeight gr - (ceiling (fontHeight gr * 3) + 50)
        scrollH = menuYStart - scrollYStart - 20
        entryH = round (fromIntegral fontSize * fontHeight gr) + lineSpace
        viewable = scrollH `div` entryH


data OpenResearchState = OpenResearchState (MenuStateInfo Int)

initOpenResearchState :: GameData -> OpenResearchState
initOpenResearchState gd = OpenResearchState $ MenuInfo 0 $ initSimpleStateInfo gd

instance GamePlayStateE OpenResearchState where
    think gps@(OpenResearchState msi) cfgs inputs = menuInfoIntThink OpenResearchState next numResearch msi inputs
        where
            gd = gamedata $ stateInfo msi
            sCfgs = sharkCfgs cfgs
            availResearch = filter (\r -> M.notMember (entryKey r) (gameDataResearchComplete gd))
                                   (getKnownResearch sCfgs gd)
            numResearch = length availResearch
            next idx
                | idx == numResearch = Step $ Transition $ AnyGamePlayState $ initResearchReviewState gd
                | otherwise = Step $ Transition $ AnyGamePlayState $ initSubmitResearchState gd (availResearch !! idx)

    transition gps@(OpenResearchState msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            gd = gamedata $ stateInfo msi
            sCfgs = sharkCfgs cfgs
            availResearch = filter (\r -> M.notMember (entryKey r) (gameDataResearchComplete gd))
                                   (getKnownResearch sCfgs gd)
            canComplete r = canCompleteResearch (getResearchRequirements sCfgs gd r)
            itemColor r = if canComplete r then Green else Blue
            items = (\r -> ScrollMenuItem (MText (getData r researchPaperName)) (itemColor r) 0 Nothing Nothing) <$> availResearch
            gview = GView (assets <> emptyNote) mempty $ Just (researchScrollMenu gr items)
            emptyNote     = if null availResearch
                            then M.singleton 3 (staticText "Find more sharks to discover research ideas" Red 80 220 2 0)
                            else M.empty
            assets = M.fromList $ zip [0..]
                [ staticText "Data Review" White 50 20 8 0
                , staticText "Open Research" LightGray 75 150 4 0
                ]

    update gps@(OpenResearchState msi) gv cfgs gr = menuInfoUpdate OpenResearchState msi gv gr id


data CompletedResearchState = CompletedResearchState (MenuStateInfo Int)

initCompletedResearchState :: GameData -> CompletedResearchState
initCompletedResearchState gd = CompletedResearchState $ MenuInfo 0 $ initSimpleStateInfo gd

instance GamePlayStateE CompletedResearchState where
    think gps@(CompletedResearchState msi) cfgs inputs = menuInfoIntThink CompletedResearchState next numResearch msi inputs
        where
            gd = gamedata $ stateInfo msi
            completedResearch = filter (\r -> M.member (entryKey r) (gameDataResearchComplete gd))
                                       (getKnownResearch (sharkCfgs cfgs) gd)
            numResearch = length completedResearch
            next idx
                | idx == numResearch = Step $ Transition $ AnyGamePlayState $ initResearchReviewState gd
                | otherwise = Step $ Transition $ AnyGamePlayState $ initCompletedResearchReviewState gd (completedResearch !! idx)

    transition gps@(CompletedResearchState msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            gd = gamedata $ stateInfo msi
            completedResearch = filter (\r -> M.member (entryKey r) (gameDataResearchComplete gd))
                                       (getKnownResearch (sharkCfgs cfgs) gd)
            items = (\r -> ScrollMenuItem (MText (getData r researchPaperName)) Blue 0 Nothing Nothing) <$> completedResearch
            gview = GView (assets <> emptyNote) mempty $ Just (researchScrollMenu gr items)
            emptyNote     = if null completedResearch
                            then M.singleton 3 (staticText "No completed research yet" Red 100 220 3 0)
                            else M.empty
            assets = M.fromList $ zip [0..]
                [ staticText "Completed Research" White 50 50 6 0
                ]

    update gps@(CompletedResearchState msi) gv cfgs gr = menuInfoUpdate CompletedResearchState msi gv gr id

data SubmitOpt = SubmitOpt | SubmitBackOpt
                deriving (Show, Eq, Ord, Enum, Bounded)

data SubmitResearchState = SubmitResearchState (DataEntryT ResearchData) (MenuStateInfo SubmitOpt)

initSubmitResearchState :: GameData -> DataEntryT ResearchData -> SubmitResearchState
initSubmitResearchState gd re = SubmitResearchState re $ initMenuStateInfo gd

instance GamePlayStateE SubmitResearchState where
    think gps@(SubmitResearchState re msi) cfgs inputs = menuInfoThink (SubmitResearchState re) next msi inputs
        where
            gd = menuGetData msi
            sCfgs = sharkCfgs cfgs
            reqs = getResearchRequirements sCfgs gd re
            canComplete = canCompleteResearch reqs
            gd' = completeResearch sCfgs gd re reqs
            next SubmitOpt = if canComplete
                                then Step $ Transition $ AnyGamePlayState $ initAwardGrantState gd' re
                                else Step NoChange
            next SubmitBackOpt = Step $ Transition $ AnyGamePlayState $ initOpenResearchState gd

    transition gps@(SubmitResearchState re msi) cfgs gr = (gview, [])
        where
            gd = gamedata $ stateInfo msi
            gview = GView assets mempty $ Just menu
            reqs = getResearchRequirements (sharkCfgs cfgs) gd re
            canComplete = canCompleteResearch reqs
            grantText   = T.append "Grant awarded when complete: $" $ T.pack $ show $ getData re researchGrant
            assets = M.fromList $ zip [0..] $
                [ staticText "Data Review" White 50 20 7 0
                , staticText (getData re researchPaperName) LightGray 50 120 5 0
                , staticText grantText Green 100 200 2 0
                ] ++ reqAssets
            (reqAssets, _) = foldl buildReq ([], 260) reqs
            buildReq (acc, y) (sN, rrs) =
                let headerA = staticText sN White 150 y 2 0
                    (rowAs, y') = foldl buildRow ([], y + 30) rrs
                in (acc ++ [headerA] ++ rowAs, y' + 20)
            buildRow (acc, y) (it, gsd, c) =
                let txt = T.concat [T.toTitle (equipTypeText it), " ", T.pack (show (length gsd)), "/", T.pack (show c)]
                    col = if length gsd >= c then Green else White
                in (acc ++ [staticText txt col 200 y 2 0], y + 30)
            completeCol = if canComplete then Blue else Gray
            menuItems = [ MenuItem (MText "Complete Research") completeCol 3 0 0 True Nothing Nothing
                        , MenuItem (MText "Back") Blue 3 0 0 True Nothing Nothing
                        ]
            menu = DefaultMenu $ MenuAsset 200 500 1 Nothing 8 menuItems

    update gps@(SubmitResearchState re msi) gv cfgs gr = menuInfoUpdate (SubmitResearchState re) msi gv gr id


data AwardGrantState = AwardGrantState (DataEntryT ResearchData) SimpleStateInfo

initAwardGrantState :: GameData -> DataEntryT ResearchData -> AwardGrantState
initAwardGrantState gd re = AwardGrantState re $ initSimpleStateInfo gd

instance GamePlayStateE AwardGrantState where
    think gps@(AwardGrantState re si) cfgs inputs = simpleInfoThink (AwardGrantState re) next si inputs
        where
            gd = gamedata si
            next = Step $ Transition $ AnyGamePlayState $ initResearchReviewState gd

    transition gps@(AwardGrantState re si) cfgs gr = (gview, [])
        where
            gview = GView assets mempty $ Just (singleMenu gr "Continue" 50 1)
            grantText = T.append "Grant awarded: $" $ T.pack $ show $ getData re researchGrant
            textW gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (0.8 :: Double)
            xStart gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (0.1 :: Double)
            stack gr' = AssetStacked $ AssetStack StackVertical
                [ StackItem (AssetText "Data Review" White 7) 0 0
                , StackItem (wrapTextStack gr' (textW gr') LightGray (getData re researchPaperName) 3 2) 0 20
                , StackItem (AssetText grantText Green 3) 0 20
                , StackItem (wrapTextStack gr' (textW gr') White (getData re researchDescription) 2 2) 0 20
                ] 4
            mainAsset gr' = Asset (stack gr') (xStart gr') 20 0 True $ Just $ \_ gr'' -> mainAsset gr''
            assets = M.singleton 0 (mainAsset gr)

    update gps@(AwardGrantState re si) gv cfgs gr = withPauseUpdate gr gps pSelM id gv
        where
            pSelM = pauseSelM si


data CompletedResearchReviewState = CompletedResearchReviewState (DataEntryT ResearchData) SimpleStateInfo

initCompletedResearchReviewState :: GameData -> DataEntryT ResearchData -> CompletedResearchReviewState
initCompletedResearchReviewState gd re = CompletedResearchReviewState re $ initSimpleStateInfo gd

instance GamePlayStateE CompletedResearchReviewState where
    think gps@(CompletedResearchReviewState re si) cfgs inputs = simpleInfoThink (CompletedResearchReviewState re) next si inputs
        where
            gd = gamedata si
            next = Step $ Transition $ AnyGamePlayState $ initCompletedResearchState gd

    transition gps@(CompletedResearchReviewState re si) cfgs gr = (gview, [])
        where
            gd = gamedata si
            gview = GView assets mempty $ Just (singleMenu gr "Back" 50 1)
            grantText = T.append "Grant awarded: $" $ T.pack $ show $ getData re researchGrant
            textW gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (0.8 :: Double)
            xStart gr' = floor $ fromIntegral (graphicsWindowWidth gr') * (0.1 :: Double)
            stack gr' = AssetStacked $ AssetStack StackVertical
                [ StackItem (AssetText "Data Review" White 7) 0 0
                , StackItem (wrapTextStack gr' (textW gr') LightGray (getData re researchPaperName) 3 2) 0 20
                , StackItem (AssetText grantText Green 3) 0 20
                , StackItem (wrapTextStack gr' (textW gr') White (getData re researchDescription) 2 2) 0 20
                ] 4
            mainAsset gr' = Asset (stack gr') (xStart gr') 20 0 True $ Just $ \_ gr'' -> mainAsset gr''
            assets = M.singleton 0 (mainAsset gr)

    update gps@(CompletedResearchReviewState re si) gv cfgs gr = withPauseUpdate gr gps pSelM id gv
        where
            pSelM = pauseSelM si
