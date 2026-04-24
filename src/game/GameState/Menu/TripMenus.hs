{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.TripMenus
    ( TripMapState(..)
    , initTripMapState
    , TripEquipPickState(..)
    , initEquipPickState
    , TripReviewState(..)
    , initTripReviewState
    , TripProgressState(..)
    , initTripProgressState
    , SharkFoundState(..)
    , TripResultState(..)
    , initTripResultState
    ) where

import SaveData
import Configs
import GameState.Types
import OutputHandles.Types
import InputState
import Util
import Shark.Types
import Shark.Trip
import Shark.Util

import Data.Map ((!))
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Maybe (isJust, isNothing)
import Data.Int (Int64)

import Graphics.Types
import Graphics.TextUtil
import Graphics.Asset
import GameState.Util

import Debug.Trace

data TripMapState = TripMapState GameData Int (Maybe PauseOpt)

initTripMapState :: GameData -> TripMapState
initTripMapState gd = TripMapState gd 0 Nothing

getLocInfo :: GameData -> GameConfigs -> Int -> [(T.Text, Bool, Bool)]
getLocInfo gd cfgs idx = locs
    where
        region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
        myBoat = gameActiveBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! myBoat
        allLocs = M.assocs (getData region siteLocations)
        locs = (\(i, (loc, lCfg)) -> (showText lCfg, loc `elem` boatReachableBiomes boatInfo, i == idx)) <$> zip [0..] allLocs


getLocAtIndex :: GameData -> GameConfigs -> Int -> Maybe (T.Text, Bool, T.Text)
getLocAtIndex gd cfgs idx = locs L.!? idx
    where
        region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
        myBoat = gameActiveBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! myBoat
        allLocs = M.assocs (getData region siteLocations)
        locs = (\(loc, lCfg) -> (loc, loc `elem` boatReachableBiomes boatInfo, biomeDescription lCfg)) <$> allLocs

getBiomeDesc :: Graphics -> GameData -> GameConfigs -> Int -> Asset
getBiomeDesc gr gd cfgs idx =
    case getLocAtIndex gd cfgs idx of
        Nothing -> Asset AssetEmpty (half gr + begBuf) 300 0 False Nothing
        Just (_, _, txt) -> biomeTextAsset txt
    where
        fontSz = 2
        sp = 10
        endBuf = 60
        begBuf = 80
        buf = begBuf + endBuf
        half gr' = graphicsWindowWidth gr' `div` 2
        rs descTxt ass gr' = ass { object = wrapTextStack gr' (half gr' - buf) White descTxt fontSz sp, assetX = half gr' + begBuf }
        biomeTextAsset descTxt = Asset (wrapTextStack gr (half gr - buf) White descTxt fontSz sp) (half gr + begBuf) 300 0 True $ Just $ rs descTxt


instance GamePlayStateE TripMapState where
    think tms@(TripMapState gd locIdx pSelM) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | enterJustPressed inputs && isNothing pSelM && locIdx == locNum = Step $ TopTransition ResearchCenterMenu gd
        | enterJustPressed inputs && isNothing pSelM =
            case currLoc of
                (key, True) -> Step $ Transition $ AnyGamePlayState $ initEquipPickState gd locIdx key
                _ -> Step NoChange
        | enterJustPressed inputs =
            case pSelM of
                Just po -> getPauseEnterAction po gd $ AnyGamePlayState $ TripMapState gd locIdx Nothing
                _ -> error "Shouldn't get here: map menu"
        | moveInputJustPressed inputs && isJust pSelM =
            case (inputDirection inputs, pSelM) of
                (Just dir, Just pSel) -> getPauseMoveAction dir pSel $ \po -> AnyGamePlayState $ TripMapState gd locIdx (Just po)
                _ -> error "Shouldn't get here: map menu: pause"
        | moveInputJustPressed inputs =
            case (inputDirection inputs, locIdx) of
                (Just DUp, 0) -> Step NoChange
                (Just DUp, n) -> stepInputUpdate $ TripMapState gd (locIdx - 1) pSelM
                (Just DDown, n) | n == locNum -> Step NoChange
                (Just DDown, n) -> stepInputUpdate $ TripMapState gd (locIdx + 1) pSelM
                _ -> Step NoChange
        | escapeJustPressed inputs && isNothing pSelM = stepInputUpdate $ TripMapState gd locIdx $ Just minBound
        | escapeJustPressed inputs = stepInputUpdate $ TripMapState gd locIdx Nothing
        | otherwise = Step $ NoChange
        where
            region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            allLocs = M.assocs (getData region siteLocations)
            locNum = length allLocs
            myBoat = gameActiveBoat $ gameDataEquipment gd
            boatInfo = boats (sharkCfgs cfgs) ! myBoat
            allowedLoc = (\(key, lcfg) -> (key, key `elem` boatReachableBiomes boatInfo)) <$> allLocs
            currLoc = allowedLoc L.!! locIdx

    transition tms@(TripMapState gd locIdx pSelM) cfgs gr = (gview, [])
        where
            overlays Nothing = mempty
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            gview = GView (M.fromList assets) (overlays pSelM) $ Just menu
            assets = zip [0..]
                         [ getBiomeDesc gr gd cfgs locIdx
                         , staticText "Select Trip" White 40 40 7 0
                         , staticText "Destination" White 80 160 8 0
                         ]
            menu = DefaultMenu $ MenuAsset 140 300 1 Nothing 8 menuItems
            locs = getLocInfo gd cfgs locIdx
            menuItems = (menuItem <$> locs) ++ [lastItem]
            menuItem (loc, enable, sel) = MenuItem (MText loc) (if enable then Blue else Gray) 3 0 0 True (if sel then Just White else Nothing) Nothing
            lastItem = MenuItem (MText "Return to Lab") Blue 3 0 0 True (if length locs == locIdx then Just White else Nothing) Nothing

    update gsp@(TripMapState gd locIdx pSelM) gvO cfgs gr = withPauseUpdate gr gsp pSelM nGv gvO
        where
            nGv gv =
                let gv' = updateDefMenuHighlight locIdx White gv
                in gv' { assets = M.adjust (\_ -> getBiomeDesc gr gd cfgs locIdx) 0 (assets gv) }


-- assuming I add ability to have more than one boat, need to select boat in new menu
-- this is done in fleet management currently

data TripEquipPickState = TripEquipPickState
    { gameData :: GameData
    , locIdx :: Int
    , locKey :: T.Text
    , menuIdx :: Int
    , equipSel :: S.Set (Int, T.Text)
    , pauseOpt :: Maybe PauseOpt
    }

initEquipPickState :: GameData -> Int -> T.Text -> TripEquipPickState
initEquipPickState gd lIdx lKey = TripEquipPickState gd lIdx lKey 0 mempty Nothing

equipLen :: GameData -> GameConfigs -> T.Text -> Int
equipLen gd cfgs loc = length $ tripAvailableEquip (sharkCfgs cfgs) gd loc

getMenuItemColor :: GameData -> GameConfigs -> T.Text -> S.Set (Int, T.Text) -> Int -> Color
getMenuItemColor gd cfgs loc eSels idx
    | idx == eqLen = if null eSels then Gray else Blue
    | idx > eqLen = Blue
    | openSlots >= iSlot = Blue
    | idx `elem` (fst <$> S.toList eSels) = Blue
    | otherwise = Gray
    where
        selKeys = snd <$> S.toList eSels
        eqLen = length availEq
        availEq = tripAvailableEquip (sharkCfgs cfgs) gd loc
        selectSlots = (\de -> equipSize (entryData de)) <$> filter (\de -> entryKey de `elem` selKeys) availEq
        boatSlots = boatEquipmentSlots (boats (sharkCfgs cfgs) ! gameActiveBoat (gameDataEquipment gd))
        openSlots = boatSlots - sum selectSlots
        iSlot = getData (availEq !! idx) equipSize

getMenuHighlight :: GameData -> GameConfigs -> T.Text -> S.Set (Int, T.Text) -> Int -> Int -> Maybe Color
getMenuHighlight gd cfgs loc eSels mIdx idx
    | mIdx == idx = Just Yellow
    | idx >= eqLen = Nothing
    | idx `elem` selIds = Just White
    | otherwise = Nothing
    where
        selIds = fst <$> S.toList eSels
        availEq = tripAvailableEquip (sharkCfgs cfgs) gd loc
        eqLen = length availEq


instance GamePlayStateE TripEquipPickState where
    think gps@(TripEquipPickState gd locIdx loc mIdx eSel pSelM) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | escapeJustPressed inputs && isNothing pSelM = openPauseMenu (\pSelM' -> gps { pauseOpt = pSelM' })
        | escapeJustPressed inputs = stepInputUpdate $ gps { pauseOpt = Nothing }
        | moveInputJustPressed inputs && isNothing pSelM =
            case (inputDirection inputs, mIdx) of
                (Just DUp, 0) -> Step NoChange
                (Just DUp, i) -> stepInputUpdate $ gps { menuIdx = i - 1 }
                (Just DDown, i) | i == eqLen + 1 -> Step NoChange
                (Just DDown, i) -> stepInputUpdate $ gps { menuIdx = i + 1 }
                _ -> Step NoChange
        | moveInputJustPressed inputs =
            case (inputDirection inputs, pSelM) of
                (Just dir, Just pSel) -> getPauseMoveAction dir pSel $ (\po -> AnyGamePlayState (gps { pauseOpt = Just po}))
                _ -> Step NoChange
        | enterJustPressed inputs && isJust pSelM =
            case pSelM of
                Just pSel -> getPauseEnterAction pSel gd $ AnyGamePlayState (gps { pauseOpt = Nothing })
                _ -> error "Can't get here:trip equip:1"
        | enterJustPressed inputs && mIdx < eqLen && S.member (mIdx, currEq) eSel =
            stepInputUpdate $ gps { equipSel = S.delete (mIdx, currEq) eSel }
        | enterJustPressed inputs && mIdx < eqLen =
            stepInputUpdate $ gps { equipSel = S.insert (mIdx, currEq) eSel }
        | enterJustPressed inputs && mIdx == eqLen && not (null eSel) = Step $ Transition $ AnyGamePlayState $ initTripReviewState gd locIdx loc eSel
        | enterJustPressed inputs && mIdx > eqLen = Step $ Transition $ AnyGamePlayState $ TripMapState gd locIdx Nothing
        | otherwise = Step NoChange
        where
            region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            eq = equipment $ sharkCfgs cfgs
            myEquip = gameOwnedEquipment $ gameDataEquipment gd
            allowedEq = allowedEquipment $ (getData region siteLocations) M.! loc
            shownEq = filter (\e -> e `elem` myEquip) allowedEq
            eqLen = length shownEq
            currEq = shownEq !! mIdx

    transition gps@(TripEquipPickState gd _ loc mIdx eSel pSelM) cfgs gr = (gv, [])
        where
            gv = GView assets (overlays pSelM) $ Just menu
            overlays Nothing = mempty
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            assets = M.fromList $ zip [0..]
                                      [ staticText equipLoadedTxt Green 200 400 3 0
                                      , staticText "Select Trip" White 50 50 8 0
                                      , staticText "Equipment" White 150 175 10 0
                                      , staticText maxSlotsTxt Green 200 350 3 0
                                      ]
            maxSlotsTxt = T.concat ["Max Slots: ", T.pack (show maxSlots), " slots"]
            equipLoadedTxt = T.concat ["Equipment Loaded: ", T.pack (show equipSlots), " slots"]
            tripAvailEq = tripAvailableEquip (sharkCfgs cfgs) gd loc
            maxSlots = boatEquipmentSlots $ boats (sharkCfgs cfgs) ! gameActiveBoat (gameDataEquipment gd)
            equipSlots = sum $ (\(_, s) -> equipSize $ (equipment (sharkCfgs cfgs)) M.! s) <$> (S.toList eSel)
            menu = DefaultMenu $ MenuAsset 250 475 1 Nothing 8 items
            itemTxts = ((\ede -> getData ede equipText ) <$> tripAvailEq) ++ ["Continue", "Back"]
            items = mkMI <$> zip [0..] itemTxts
            mkMI (i, it) = MenuItem (MText it) (getMenuItemColor gd cfgs loc eSel i) 3 0 0 True (getMenuHighlight gd cfgs loc eSel mIdx i) Nothing

    update gps@(TripEquipPickState gd _ loc mIdx eSel pSelM) gvO cfgs gr = withPauseUpdate gr gps pSelM (nGv . updateLoaded) gvO
        where
            equipSlots = sum $ (\(_, s) -> equipSize $ (equipment (sharkCfgs cfgs)) M.! s) <$> (S.toList eSel)
            equipLoadedTxt = T.concat ["Equipment Loaded: ", T.pack (show equipSlots), " slots"]
            updateLoaded gv = gv { assets = M.adjust (\a -> a { object = AssetText equipLoadedTxt Green 3 }) 0 (assets gv) }
            nGv = updateDefaultMenu (\i mi -> mi { menuItemColor = getMenuItemColor gd cfgs loc eSel i
                                                 , highlightedColor = getMenuHighlight gd cfgs loc eSel mIdx i
                                                 })

data TripReviewOpts = StartTripRv | BackEquipRv | AbortTripRv
                    deriving (Show, Eq, Enum, Ord, Bounded)

data TripReviewState = TripReviewState
    { reviewGameData :: GameData
    , reviewLoc :: (Int, T.Text)
    , reviewEquip :: S.Set (Int, T.Text)
    , reviewMenuIdx :: TripReviewOpts
    , reviewPauseOpt :: Maybe PauseOpt
    }

initTripReviewState :: GameData -> Int -> T.Text -> S.Set (Int, T.Text) -> TripReviewState
initTripReviewState gd locIdx locKey equips = TripReviewState gd (locIdx, locKey) equips minBound Nothing

instance GamePlayStateE TripReviewState where
    think gps@(TripReviewState gd (locI, loc) equip mIdx pSelM) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | escapeJustPressed inputs && isNothing pSelM = openPauseMenu (\pSelM' -> gps { reviewPauseOpt = pSelM' })
        | escapeJustPressed inputs = stepInputUpdate $ gps { reviewPauseOpt = Nothing }
        | isJust pSelM && (enterJustPressed inputs || moveInputJustPressed inputs) =
            case (inputDirection inputs, enterJustPressed inputs, pSelM) of
                (Just dir, _, Just pSel) -> getPauseMoveAction dir pSel $ (\po -> AnyGamePlayState (gps { reviewPauseOpt = Just po}))
                (_, True, Just pSel) -> getPauseEnterAction pSel gd $ AnyGamePlayState (gps { reviewPauseOpt = Nothing })
                _ -> Step NoChange
        | enterJustPressed inputs =
            case (mIdx, tripAInfo) of
                (StartTripRv, Just (atmpt, tp)) -> Step $ Transition $ AnyGamePlayState $ initTripProgressState gd'' atmpt tp $ timestamp inputs
                (BackEquipRv, _) -> Step $ Transition $ AnyGamePlayState $ initEquipPickState gd locI loc
                (AbortTripRv, _) -> Step $ TopTransition ResearchCenterMenu gd
        | otherwise =
            case keyInputs inputs of
                (Just (Keyboard _ (Just dir) _ False)) -> moveMenuPos dir mIdx (\newMI -> (gps { reviewMenuIdx = newMI }))
                _ -> Step NoChange
        where
            equipKeys = snd <$> S.toList equip
            region = gameCurrentRegion gd
            boat = gameActiveBoat $ gameDataEquipment gd
            trip = tripInfo (sharkCfgs cfgs) region loc boat equipKeys
            funds = gameDataFunds gd
            tc = tripCost trip
            gd' = gd { gameDataFunds = funds - tc, gameDataMonth = gameDataMonth gd + tripLength trip }
            (gd'', atmpts) = initTripProgress gd' (gameCurrentRegion gd) loc boat equipKeys cfgs
            tripAInfo = case tripTries atmpts of
                [] -> Nothing
                (h:tl) -> Just (h, atmpts { tripTries = tl})

    transition gps@(TripReviewState gd (_, loc) equip mIdx pSelM) cfgs gr = (gview, [])
        where
            gview = GView assets (overlays pSelM) $ Just menu
            overlays Nothing = mempty
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            assets = M.fromList $ zip [0..]
                                      [ staticText "Review Trip" White 50 20 8 0
                                      , staticText "Details" White 150 125 10 0
                                      , staticText boatTxt White 300 280 3 0
                                      , staticText fuelCostTxt White 305 325 3 0
                                      , Asset stack 250 425 0 True Nothing
                                      ]
            boatTxt = T.append "Boat: " (boatName boatInfo)
            fuelCost = boatFuelCost boatInfo
            fuelCostTxt = T.append "Fuel Cost: " (showMoney fuelCost)
            menu = DefaultMenu $ MenuAsset 400 600 1 Nothing 5 items
            itemHL i = if fromEnum mIdx == i then Just White else Nothing
            items = [ MenuItem (MText "Start Trip") Blue 3 0 0 True (itemHL 0) Nothing
                    , MenuItem (MText "Back to equipment") Blue 3 0 0 True (itemHL 1) Nothing
                    , MenuItem (MText "Abort Trip") Blue 3 0 0 True (itemHL 2) Nothing
                    ]
            region = gameCurrentRegion gd
            boat = gameActiveBoat $ gameDataEquipment gd
            boatInfo = boats (sharkCfgs cfgs) ! boat
            trip = tripInfo (sharkCfgs cfgs) region loc boat $ snd <$> S.toList equip
            funds = gameDataFunds gd
            tc = tripCost trip
            costTxts = zip [Green, Red, if funds >= tc then Green else Red] $
                           splitJustSpacing [ ("Current Funds: ", showMoney funds)
                                            , ("Trip Cost: ", showMoney tc)
                                            , ("After Trip: ", showMoney (funds - tc))
                                            ]
            stack = AssetStacked $ AssetStack StackVertical stackItems 8
            stackItems = (\(c, txt) -> StackItem (AssetText txt c 3) 0 0) <$> costTxts

    update gps@(TripReviewState gd (_, loc) equip mIdx pSelM) gvO cfgs gr = withPauseUpdate gr gps pSelM (updateDefMenuHighlight (fromEnum mIdx) White) gvO

data TripProgressState = TripProgressState GameData TripAttempt TripState Int64 [AnimationState]

initTripProgressState :: GameData -> TripAttempt -> TripState -> Int64 -> TripProgressState
initTripProgressState gd ta trip ts = TripProgressState gd ta trip ts []

newTrip :: GameData -> TripState -> Maybe SharkFind -> [TripAttempt] -> TripState
newTrip gd'' tp sfM tl = case sfM of
                            Nothing -> tp { tripTries = tl }
                            Just sf -> tp { tripTries = tl, sharkFinds = sharkFinds tp ++ [sf]}

instance GamePlayStateE TripProgressState where
    think gps@(TripProgressState gd ta tp startTS as) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | timestamp inputs - startTS > 2000 = Step $ Transition $ AnyGamePlayState $ SharkFoundState gd' sfM tp'
        | otherwise = Step $ getAnimationStep inputs as
        where
            (gd', sfM, tp') =
                let (gdNew, sfM') = executeTrip (sharkCfgs cfgs) gd (trip tp) ta
                in (gdNew, sfM', newTrip gdNew tp sfM' (tripTries tp))

    transition gps@(TripProgressState gd ta trip ts _) cfgs gr = (gview, [as])
        where
            gview = GView assets mempty Nothing
            assets = M.fromList $ zip [0..]
                                      [ centerAssetX gr (AssetAnimation "skate" 0 0 5.0) 0 350 0
                                      , staticText "Trip Progress" White 50 50 8 0
                                      , staticText "Looking for sharks..." Blue 100 200 4 0
                                      , staticText equipTxt Green 150 280 3 0
                                      , backRect
                                      , progressRect
                                      ]
            curA = 1 + (length $ tripTries trip)
            allA = tripTotalTries trip
            skateUp a@(Asset (AssetAnimation img f d scale) _ _ _ _ rs) gr' =
                let a' = maybe a (\rsF -> rsF a gr') rs
                    newF = mod (f + 1) $ animFrameCount $ graphicsAnimTextures gr' M.! img
                in a' { object = AssetAnimation img newF d scale }
            as = AnimState 0 150 [0] skateUp
            progX gr' = midStartX gr' $ progW gr'
            -- In the future probably want to make the y dynamic too
            progY = 650
            progH = 80
            progW gr' = max 1000 $ percentWidth gr' 0.8
            backRect = Asset (AssetRect (progW gr) progH Gray) (progX gr) progY 1 True $ Just resizeBR
            resizeBR a gr' = a { object = AssetRect (progW gr') progH Gray, assetX = progX gr' }
            p = floor (fromIntegral (allA - curA) / fromIntegral allA * 100)
            progressRect = Asset (AssetRect (((progW gr) `div` 100) * p) progH Green) (progX gr) progY 2 True $ Just resizePR
            resizePR a gr' = a { object = AssetRect (((progW gr') `div` 100) * p) progH Green, assetX = progX gr'}
            equipTxt = T.append "Using " $ getData (attemptEquipment ta) equipText

    updateAnims (TripProgressState gd a trip ts _) anims = TripProgressState gd a trip ts anims


data SharkFoundState = SharkFoundState GameData (Maybe SharkFind) TripState

instance GamePlayStateE SharkFoundState where
    think gps@(SharkFoundState gd sfM tp) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | enterJustPressed inputs =
            case tripAInfo of
                (Just (a, tp')) -> Step $ Transition $ AnyGamePlayState $ initTripProgressState gd a tp' $ timestamp inputs
                Nothing -> Step $ Transition $ AnyGamePlayState $ initTripResultState gd tp
        | otherwise = Step NoChange
        where
            tripAInfo = case tripTries tp of
                [] -> Nothing
                (h:tl) -> Just (h, tp { tripTries = tl})

    transition gps@(SharkFoundState gd sfM trip) cfgs gr = (gview, [])
        where
            gview = GView assets mempty $ Just $ singleMenu gr "Continue Trip" 30 0
            assets = M.fromList $ zip [0..] ass
            ass = case sfM of
                    Just sf -> foundShark gd sf trip cfgs gr
                    Nothing -> noShark gr

foundShark :: GameData -> SharkFind -> TripState -> GameConfigs -> Graphics -> [Asset]
foundShark gd sf trip cfgs gr =
    [ staticText typeText White 60 20 8 0
    , staticText sharkText Blue 200 150 5 0
    , centerScaleImgX gr sharkImgKey 265 100 220 1.5 0
    ]
    where
        typeText = T.concat ["You ", infoTypeText, " a "]
        sharkText = getData (findSpecies sf) sharkName
        sharkImgKey = getData (findSpecies sf) sharkImage
        infoTypeText = equipTypeText $ getData (findEquipment sf) equipInfoType

noShark :: Graphics -> [Asset]
noShark gr = [ staticText "No Shark" White 50 20 10 0
             , staticText "Found" White 150 150 8 0
             , staticText "Better luck next time!" White 200 280 4 0
             , centerScaleImgX gr "empty_net" 315 100 250 1.2 0
             ]


data TripResultState = TripResultState
    { resGameData :: GameData
    , resTrip :: TripState
    , resPosition :: Int
    , resPauseOpt :: Maybe PauseOpt
    }

initTripResultState :: GameData -> TripState -> TripResultState
initTripResultState gd tp = TripResultState gd tp 0 Nothing


instance GamePlayStateE TripResultState where
    think gps@(TripResultState gd tp pos pSelM) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | isJust pSelM && (enterJustPressed inputs || moveInputJustPressed inputs) =
            case (inputDirection inputs, enterJustPressed inputs, pSelM) of
                (_, True, Just pSel) -> getPauseEnterAction pSel gd' $ AnyGamePlayState (gps { resPauseOpt = Nothing })
                (Just dir, _, Just pSel) -> getPauseMoveAction dir pSel $ (\po -> AnyGamePlayState (gps { resPauseOpt = Just po}))
                _ -> Step NoChange
        | escapeJustPressed inputs =
            case pSelM of
                Nothing -> stepInputUpdate $ (TripResultState gd tp pos (Just minBound))
                _ -> stepInputUpdate $ (TripResultState gd tp pos Nothing)
        | enterJustPressed inputs = Step $ TopTransition ResearchCenterMenu gd'
        | otherwise =
            case mouseInputs inputs of
                Nothing -> Step NoChange
                (Just (MouseInputs sAmt)) -> stepInputUpdate $ TripResultState gd tp (max 0 (pos + 5 * (-sAmt))) pSelM
        where
            gd' = foldl (\g sf -> addShark g (mkGameShark sf)) gd (sharkFinds tp)

    transition gps@(TripResultState gd tp pos pSelM) cfgs gr = (gview, [])
        where
            gview = GView assets (overlays pSelM) $ Just $ singleMenu gr "Back to Research Center" 50 1
            overlays Nothing = mempty
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            assets = M.fromList $ zip [0..]
                                      [ Asset (scroll gr) 200 200 0 True $ Just rs
                                      , staticText "Trip Complete!" White 50 50 8 0
                                      ]
            scrollH gr' = graphicsWindowHeight gr' - 400
            rs as gr' = as { object = scroll gr' }
            scroll gr' = AssetScroll $ ScrollObj subAssets pos $ scrollH gr'
            subAssets = M.fromList $ zip [0..] sharkFindsTxt
            findAssets (i, sf) = [ Asset (AssetText (findText sf) White 3) 10 (i * 75) 0 True Nothing
                               -- , TextDisplay (T.append "at " (monthToText (findMonth sf))) 200 (300 + (i * 100)) 3 White
                               ]
            findText sf = T.concat ["- ", getData (findSpecies sf) sharkName, " ", infoTypeText]
                where
                    infoTypeText = case getData (findEquipment sf) equipInfoType of
                        Caught -> "caught"
                        Observed -> "observed"
            sharkFindsTxt = concatMap findAssets $ zip [0..] (sharkFinds tp)


    update gps@(TripResultState gd tp p pSelM) gvO _ gr = withPauseUpdate gr newGPS pSelM updatePos gvO
        where
            newGPS = TripResultState gd tp (min p maxP) pSelM
            maxP = maybe 0 (getMaxPosition gr) (getAScroll gvO)
            getAScroll gv =
                case object (assets gv M.! 0) of
                    AssetScroll sc -> Just sc
                    _ -> Nothing
            updatePos gv = gv { assets = M.adjust updateScA 0 (assets gv) }
            updateScA as =
                case object as of
                    AssetScroll sc -> as { object = AssetScroll $ updateScrollPosition gr sc p }
                    _ -> as
