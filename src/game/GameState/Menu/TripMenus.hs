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
import Data.Maybe (isJust)
import Data.Int (Int64)

import Graphics.Types
import Graphics.TextUtil
import Graphics.Asset
import GameState.Util

import Debug.Trace

data TripMapState = TripMapState (MenuStateInfo Int)

initTripMapState :: GameData -> TripMapState
initTripMapState gd = TripMapState $ MenuInfo 0 $ initSimpleStateInfo gd

getLocInfo :: GameData -> GameConfigs -> [(T.Text, Bool)]
getLocInfo gd cfgs = locs
    where
        region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
        myBoat = gameActiveBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! myBoat
        allLocs = M.assocs (getData region siteLocations)
        locs = (\(loc, lCfg) -> (showText lCfg, loc `elem` boatReachableBiomes boatInfo)) <$> allLocs


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
    think tms@(TripMapState msi) cfgs inputs = menuInfoIntThink TripMapState next back locNum msi inputs
        where
            gd = menuGetData msi
            locIdx = menuSel msi
            region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            allLocs = M.assocs (getData region siteLocations)
            locNum = length allLocs
            myBoat = gameActiveBoat $ gameDataEquipment gd
            boatInfo = boats (sharkCfgs cfgs) ! myBoat
            allowedLoc = (\(key, lcfg) -> (key, key `elem` boatReachableBiomes boatInfo)) <$> allLocs
            currLoc = allowedLoc L.!! locIdx
            back = Step $ TopTransition ResearchCenterMenu gd
            next idx
                | idx == locNum = back
                | otherwise = case currLoc of
                                (key, True) -> stepTransition $ initEquipPickState gd locIdx key
                                _ -> Step NoChange

    transition tms@(TripMapState msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            pSelM = menuGetPause msi
            gd = menuGetData msi
            locIdx = menuSel msi
            gview = GView (M.fromList assets) mempty $ Just menu
            assets = zip [0..]
                         [ getBiomeDesc gr gd cfgs locIdx
                         , staticText "Select Trip" White 40 40 7 0
                         , staticText "Destination" White 80 160 8 0
                         ]
            menu = ScrollMenu $ mkScrollMenu gr menuItems endItems (const 100) 140 300 8 3 1
            endItems = [MenuItem (MText "Return to Lab") Blue 3 0 0 True Nothing Nothing]
            locs = getLocInfo gd cfgs
            menuItems = menuItem <$> locs
            menuItem (loc, enable) = ScrollMenuItem (MText loc) (if enable then Blue else Gray) 0 Nothing Nothing

    update gsp@(TripMapState msi) gv cfgs gr = menuInfoUpdate TripMapState msi gv gr id


-- assuming I add ability to have more than one boat, need to select boat in new menu
-- this is done in fleet management currently

data TripEquipPickState = TripEquipPickState
    { locIdx :: Int
    , locKey :: T.Text
    , equipSel :: S.Set (Int, T.Text)
    , equipMenuInfo :: MenuStateInfo Int
    }

initEquipPickState :: GameData -> Int -> T.Text -> TripEquipPickState
initEquipPickState gd lIdx lKey = TripEquipPickState lIdx lKey mempty $ MenuInfo 0 $ initSimpleStateInfo gd

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
    think gps@(TripEquipPickState locIdx loc eSel msi) cfgs inputs =
        menuInfoIntThink (\msi' -> gps { equipMenuInfo = msi' }) next back (eqLen + 1) msi inputs
        where
            gd = menuGetData msi
            region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            eq = equipment $ sharkCfgs cfgs
            myEquip = gameOwnedEquipment $ gameDataEquipment gd
            allowedEq = allowedEquipment $ (getData region siteLocations) M.! loc
            shownEq = filter (\e -> e `elem` myEquip) allowedEq
            eqLen = length shownEq
            back = stepTransition $ TripMapState $ MenuInfo locIdx $ SimpleInfo gd Nothing
            next idx
                | idx > eqLen = back
                | idx == eqLen = stepTransition $ initTripReviewState gd locIdx loc eSel
                | S.member (idx, shownEq !! idx) eSel = stepInputUpdate $ gps { equipSel = S.delete (idx, shownEq !! idx) eSel }
                | otherwise = stepInputUpdate $ gps { equipSel = S.insert (idx, shownEq !! idx) eSel }

    transition gps@(TripEquipPickState _ loc eSel msi) cfgs gr = (gv, [])
        where
            gd = menuGetData msi
            mIdx = menuSel msi
            gv = GView assets mempty $ Just menu
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

    update gps@(TripEquipPickState _ loc eSel msi) gvO cfgs gr = withPauseUpdate gr gps pSelM (nGv . updateLoaded) gvO
        where
            gd = menuGetData msi
            mIdx = menuSel msi
            pSelM = menuGetPause msi
            equipSlots = sum $ (\(_, s) -> equipSize $ (equipment (sharkCfgs cfgs)) M.! s) <$> (S.toList eSel)
            equipLoadedTxt = T.concat ["Equipment Loaded: ", T.pack (show equipSlots), " slots"]
            updateLoaded gv = gv { assets = M.adjust (\a -> a { object = AssetText equipLoadedTxt Green 3 }) 0 (assets gv) }
            nGv = updateDefaultMenu (\i mi -> mi { menuItemColor = getMenuItemColor gd cfgs loc eSel i
                                                 , highlightedColor = getMenuHighlight gd cfgs loc eSel mIdx i
                                                 })

data TripReviewOpts = StartTripRv | BackEquipRv | AbortTripRv
                    deriving (Show, Eq, Enum, Ord, Bounded)

data TripReviewState = TripReviewState
    { reviewLoc :: (Int, T.Text)
    , reviewEquip :: S.Set (Int, T.Text)
    , reviewMenuInfo :: MenuStateInfo TripReviewOpts
    }

initTripReviewState :: GameData -> Int -> T.Text -> S.Set (Int, T.Text) -> TripReviewState
initTripReviewState gd locIdx locKey equips = TripReviewState (locIdx, locKey) equips $ initMenuStateInfo gd

instance GamePlayStateE TripReviewState where
    think gps@(TripReviewState (locI, loc) equip msi) cfgs inputs = menuInfoThink (\msi' -> gps { reviewMenuInfo = msi' }) (next tripAInfo) back msi inputs
        where
            gd = menuGetData msi
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
            back = Step $ TopTransition ResearchCenterMenu gd
            next _ AbortTripRv = back
            next _ BackEquipRv = stepTransition $ initEquipPickState gd locI loc
            next (Just (atmpt, tp)) StartTripRv = stepTransition $ initTripProgressState gd'' atmpt tp $ timestamp inputs
            next _ _ = Step NoChange

    transition gps@(TripReviewState (_, loc) equip msi) cfgs gr = (menuInfoApply msi gview gr id, [])
        where
            gd = menuGetData msi
            gview = GView assets mempty $ Just menu
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
            items = [ MenuItem (MText "Start Trip") Blue 3 0 0 True Nothing Nothing
                    , MenuItem (MText "Back to equipment") Blue 3 0 0 True Nothing Nothing
                    , MenuItem (MText "Abort Trip") Blue 3 0 0 True Nothing Nothing
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

    update gps@(TripReviewState (_, loc) equip msi) gv cfgs gr = menuInfoUpdate (\msi' -> gps { reviewMenuInfo = msi' }) msi gv gr id

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
        | timestamp inputs - startTS > 2000 = stepTransition $ SharkFoundState gd' sfM tp'
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
                (Just (a, tp')) -> stepTransition $ initTripProgressState gd a tp' $ timestamp inputs
                Nothing -> stepTransition $ initTripResultState gd tp
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
    { resTrip :: TripState
    , resPosition :: Int
    , resInfo :: SimpleStateInfo
    }

initTripResultState :: GameData -> TripState -> TripResultState
initTripResultState gd tp = TripResultState tp 0 $ initSimpleStateInfo gd


instance GamePlayStateE TripResultState where
    think gps@(TripResultState tp pos si) cfgs inputs =
        case mouseInputs inputs of
            Nothing -> simpleInfoThink (TripResultState tp pos) enterPressed si inputs
            Just (MouseInputs sAmt) -> stepInputUpdate $ TripResultState tp (max 0 (pos + 5 * (-sAmt))) si
        where
            gd = gamedata si
            gd' = foldl (\g sf -> addShark g (mkGameShark sf)) gd (sharkFinds tp)
            enterPressed = Step $ TopTransition ResearchCenterMenu gd'

    transition gps@(TripResultState tp pos si) cfgs gr = (withPauseApply gr (pauseSelM si) gview, [])
        where
            gview = GView assets mempty $ Just $ singleMenu gr "Back to Research Center" 50 1
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


    update gps@(TripResultState tp p si) gvO _ gr = withPauseUpdate gr newGPS (pauseSelM si) updatePos gvO
        where
            newGPS = TripResultState tp (min p maxP) si
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
