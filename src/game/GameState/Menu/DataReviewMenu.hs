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

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Text as T

import Util
import OutputHandles.Types
import Configs
import Shark.Types
import Shark.Review
import Shark.Util
import GameState.Types
import GameState.Util
import SaveData
import Graphics.Types
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
    think gps@(TopReviewState msi) cfgs inputs = menuInfoThink TopReviewState next back msi inputs
        where
            gd = gamedata $ stateInfo msi
            back = Step $ TopTransition ResearchCenterMenu gd
            next ReviewSharkOpt = stepTransition $ initSharkReviewTopState gd
            next ReviewResearchOpt = stepTransition $ initResearchReviewState gd
            next ReviewReturnOpt = back

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
        gview = GView assets mempty $ Just (ScrollMenu menu)
        assets = M.fromList $ zip [0..]
                                  [ staticText "Data Review" White 50 20 8 0
                                  , staticText "Discovered Sharks" Green 75 150 4 0
                                  ]
        sharkEntries = sharkInfo gd cfgs
        items = (\(Entry _ e) -> ScrollMenuItem (MText (sharkName e)) Blue 0 Nothing Nothing) <$> sharkEntries
        endItems = [MenuItem (MText "Back") Blue fs 0 0 True Nothing Nothing]
        menu = mkScrollMenu gr items endItems (const 100) 100 300 ls fs 0

instance GamePlayStateE SharkReviewTopState where
    think gps@(SharkReviewTopState msi) cfgs inputs = menuInfoIntThink newGPS chooseShark back (length sharks) msi inputs
        where
            gd = gamedata $ stateInfo msi
            sharks = sharkInfo gd cfgs
            newGPS = SharkReviewTopState
            back = stepTransition $ initTopReviewState gd
            chooseShark idx
                | idx == length sharks = back
                | otherwise = stepTransition $ initSharkReviewState gd $ sharks L.!! idx

    transition gps@(SharkReviewTopState msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            gview = initSharkTopView gd cfgs gr
            gd = gamedata $ stateInfo msi

    update gps@(SharkReviewTopState msi) gv cfgs gr = menuInfoUpdate SharkReviewTopState msi gv gr id


buildSharkFactStack :: Graphics -> GameData -> DataEntryT SharkInfo -> AssetObj
buildSharkFactStack gr gd sharkEntry = AssetStacked $ AssetStack StackVertical (concatMap factItems facts) 15
    where
        facts = getFactsFound gd sharkEntry
        descW = graphicsWindowWidth gr - 250
        factItems fact =
            [ StackItem (AssetText (sharkFactTitle fact) Yellow 3) 50 0
            , StackItem (wrapTextStack gr descW White (sharkFactInfo fact) 2 3) 100 0
            ]

data SharkReviewState = SharkReviewState
    { srsEntry ::(DataEntryT SharkInfo)
    , srsScPos :: Int
    , srsInfo :: SimpleStateInfo
    }

initSharkReviewState :: GameData -> DataEntryT SharkInfo -> SharkReviewState
initSharkReviewState gd se = SharkReviewState se 0 $ initSimpleStateInfo gd

defaultSharkReview :: DataEntryT SharkInfo -> GameData -> GameConfigs -> Graphics -> GView
defaultSharkReview sharkEntry gd cfgs gr = GView assets mempty $ Just (singleMenu gr "Back" 50 1)
    where
        sCfgs = sharkCfgs cfgs
        imgKey = getData sharkEntry sharkImage
        locsMap = getSeenLocations sCfgs gd sharkEntry
        sharkFinds = getFinds sCfgs gd sharkEntry
        infoCnts = getInfoCounts sharkFinds
        sightingText = T.append "Shark interactions: " $ T.pack $ show $ length sharkFinds
        countText (infoType, cnt) = T.concat [T.toTitle (equipTypeText infoType), ": ", T.pack (show cnt)]
        siteW gr' = graphicsWindowWidth gr' - 150
        infoStack gr' = AssetStacked $ AssetStack StackVertical
            (  [ StackItem (AssetText (getData sharkEntry sharkName) White 5) 0 0
               , StackItem (AssetText "Locations Seen:" Gray 4) 45 0
               ]
            ++ concatMap (regionItems gr') (M.toList locsMap)
            ++ [StackItem (AssetText sightingText White 2) 60 10]
            ++ ((\c -> StackItem (AssetText (countText c) White 2) 110 0) <$> M.toList infoCnts)
            ) 10
        regionItems gr' (region, sites) =
            [ StackItem (AssetText region Blue 3) 55 0
            , StackItem (wrapTextStack gr' (siteW gr') Green (T.intercalate ", " sites) 2 3) 80 0
            ]
        infoA = Asset (infoStack gr) 20 20 0 True $ Just $ \a gr' -> a { object = infoStack gr' }
        maxImgScale = 1.5 -- pixel art starts looking bad scaled larger than this
        imgScale gr' = min maxImgScale (fromIntegral (graphicsWindowWidth gr') / 900.0)
        imgInfo gr' = graphicsStaticTextures gr' M.! imgKey
        imgW gr' = floor $ fromIntegral (imageSizeX (imgInfo gr')) * imgScale gr'
        imgX gr' = graphicsWindowWidth gr' - imgW gr' - 30
        imgRs a gr' = a { object = AssetImage imgKey (imgScale gr'), assetX = imgX gr' }
        imgA = Asset (AssetImage imgKey (imgScale gr)) (imgX gr) 200 1 True (Just imgRs)
        scrollYStart gr' = 20 + assetObjHeight gr' (infoStack gr') + 20
        optYStart gr' = graphicsWindowHeight gr' - 70
        scrollH gr' = optYStart gr' - 5 - scrollYStart gr'
        factAssets gr' = M.singleton 0 $ Asset (buildSharkFactStack gr' gd sharkEntry) 0 0 0 True Nothing
        scrollRs a gr' = a { assetY = scrollYStart gr'
                           , object = AssetScroll $ case object a of
                               AssetScroll (ScrollObj _ pos _) -> ScrollObj (factAssets gr') pos (scrollH gr')
                               _ -> ScrollObj (factAssets gr') 0 (scrollH gr') }
        scrollA = Asset (AssetScroll $ ScrollObj (factAssets gr) 0 (scrollH gr)) 50 (scrollYStart gr) 0 True (Just scrollRs)
        assets = M.fromList $ zip [0..] [infoA, imgA, scrollA]

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
            enterPressed = stepTransition $ SharkReviewTopState $ MenuInfo restoredIdx $ SimpleInfo gd Nothing

    transition gps@(SharkReviewState sharkEntry _ si) cfgs gr = (applySharkReview gps (defaultSharkReview sharkEntry (gamedata si) cfgs gr) cfgs gr, [])

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
    think gps@(ResearchReviewState msi) cfgs inputs = menuInfoThink ResearchReviewState next back msi inputs
        where
            gd = menuGetData msi
            back = stepTransition $ initTopReviewState gd
            next ResearchOpenOpt = stepTransition $ initOpenResearchState gd
            next ResearchCompleteOpt = stepTransition $ initCompletedResearchState gd
            next ResearchBackOpt = back

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


researchScrollMenu :: Graphics -> [ScrollMenuItem] -> AssetMenu
researchScrollMenu gr items = ScrollMenu $ mkScrollMenu gr items endItems (const 100) 50 240 8 3 0
    where
        endItems = [MenuItem (MText "Back") Blue 3 0 0 True Nothing Nothing]

data OpenResearchState = OpenResearchState (MenuStateInfo Int)

initOpenResearchState :: GameData -> OpenResearchState
initOpenResearchState gd = OpenResearchState $ MenuInfo 0 $ initSimpleStateInfo gd

instance GamePlayStateE OpenResearchState where
    think gps@(OpenResearchState msi) cfgs inputs = menuInfoIntThink OpenResearchState next back numResearch msi inputs
        where
            gd = gamedata $ stateInfo msi
            sCfgs = sharkCfgs cfgs
            availResearch = filter (\r -> M.notMember (entryKey r) (gameDataResearchComplete gd))
                                   (getKnownResearch sCfgs gd)
            numResearch = length availResearch
            back = stepTransition $ initResearchReviewState gd
            next idx
                | idx == numResearch = back
                | otherwise = stepTransition $ initSubmitResearchState gd (availResearch !! idx)

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
    think gps@(CompletedResearchState msi) cfgs inputs = menuInfoIntThink CompletedResearchState next back numResearch msi inputs
        where
            gd = gamedata $ stateInfo msi
            completedResearch = filter (\r -> M.member (entryKey r) (gameDataResearchComplete gd))
                                       (getKnownResearch (sharkCfgs cfgs) gd)
            numResearch = length completedResearch
            back = stepTransition $ initResearchReviewState gd
            next idx
                | idx == numResearch = back
                | otherwise = stepTransition $ initCompletedResearchReviewState gd (completedResearch !! idx)

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
    think gps@(SubmitResearchState re msi) cfgs inputs = menuInfoThink (SubmitResearchState re) next back msi inputs
        where
            gd = menuGetData msi
            sCfgs = sharkCfgs cfgs
            reqs = getResearchRequirements sCfgs gd re
            canComplete = canCompleteResearch reqs
            gd' = completeResearch sCfgs gd re reqs
            back = stepTransition $ initOpenResearchState gd
            next SubmitOpt = if canComplete
                                then stepTransition $ initAwardGrantState gd' re
                                else Step NoChange
            next SubmitBackOpt = back

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
            next = stepTransition $ initResearchReviewState gd

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
            next = stepTransition $ initCompletedResearchState gd

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
