{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.TripMenus
    ( initEquipPickState
    , equipmentPickMenu
    , reviewTripMenu
    , tripProgressMenu
    , sharkFoundMenu
    , tripResultsMenu
    , initTripMapState
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
import Data.Maybe (isJust, fromJust, isNothing)

import Graphics.Types
import Graphics.Menu
import Graphics.TextUtil
import Graphics.ImageUtil
import Graphics.Animation
import Graphics.NewTypes
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
getBiomeDesc gr gd cfgs idx = case getLocAtIndex gd cfgs idx of
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
                (Just DUp, n) -> Step $ InputUpdate $ AnyGamePlayState $ TripMapState gd (locIdx - 1) pSelM
                (Just DDown, n) | n == locNum -> Step NoChange
                (Just DDown, n) -> Step $ InputUpdate $ AnyGamePlayState $ TripMapState gd (locIdx + 1) pSelM
                _ -> Step NoChange
        | escapeJustPressed inputs && isNothing pSelM = Step $ InputUpdate $ AnyGamePlayState $ TripMapState gd locIdx $ Just minBound
        | escapeJustPressed inputs = Step $ InputUpdate $ AnyGamePlayState $ TripMapState gd locIdx Nothing
        | otherwise = Step $ NoChange
        where
            region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            allLocs = M.assocs (getData region siteLocations)
            locNum = length allLocs
            myBoat = gameActiveBoat $ gameDataEquipment gd
            boatInfo = boats (sharkCfgs cfgs) ! myBoat
            allowedLoc = (\(key, lcfg) -> (key, key `elem` boatReachableBiomes boatInfo)) <$> allLocs
            currLoc = allowedLoc L.!! locIdx

    transition tms@(TripMapState gd locIdx pSelM) cfgs gr = GameStateNew (AnyGamePlayState (TripMapState gd locIdx pSelM)) gview
        where
            pause = pauseOverlay gr pSelM
            gview = GView (M.fromList assets) (M.singleton 0 pause) [] $ Just menu
            assets = zip [0..]
                         [ getBiomeDesc gr gd cfgs locIdx
                         , staticText "Select Trip" White 40 40 7 0
                         , staticText "Destination" White 80 160 8 0
                         ]
            menu = MenuAsset 140 300 1 Nothing False 8 menuItems
            locs = getLocInfo gd cfgs locIdx
            menuItems = (menuItem <$> locs) ++ [lastItem]
            menuItem (loc, enable, sel) = MenuItem loc (if enable then Blue else Gray) 3 0 0 (if sel then Just White else Nothing) Nothing
            lastItem = MenuItem "Return to Lab" Blue 3 0 0 (if length locs == locIdx then Just White else Nothing) Nothing

    update gsp@(TripMapState gd locIdx pSelM) gsn cfgs gr = withPauseUpdate gsp pSelM nGv gsn
        where
            nGv gv =
                let gv' = updateMenuHighlight locIdx White gv
                in gv' { assets = M.adjust (\_ -> getBiomeDesc gr gd cfgs locIdx) 0 (assets gv) }


-- assuming I add ability to have more than one boat, need to select boat in new menu
-- this is done in fleet management currently

--data EquipMenuOpt = Equip T.Text | 

data TripEquipPickState = TripEquipPickState
    { gameData :: GameData
    , locIdx :: Int
    , locKey :: T.Text
    , menuIdx :: Int
    , equipSel :: [(Int, T.Text)]
    , pauseOpt :: Maybe PauseOpt
    }

initEquipPickState :: GameData -> Int -> T.Text -> TripEquipPickState
initEquipPickState gd lIdx lKey = TripEquipPickState gd lIdx lKey 0 [] Nothing

instance GamePlayStateE TripEquipPickState where
    think gps@(TripEquipPickState gd _ _ mIdx eSel pSelM) cfgs inputs = Step NoChange

    transition gps cfgs gr = GameStateNew (AnyGamePlayState gps) gv
        where
            gv = GView mempty mempty [] Nothing

equipmentPickMenu :: GameData -> (Int, T.Text) -> [T.Text] -> Int -> GameConfigs -> GameMenu
equipmentPickMenu gd (idx, loc) chsn pos cfgs = GameMenu (textView words) (Menu (selMultOpts 250 475 3 8 opts' update act (Just back) pos) Nothing)
    where
        region = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
        myEquip = gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! gameActiveBoat myEquip
        slots = boatEquipmentSlots boatInfo
        allowedEq = allowedEquipment $ (getData region siteLocations) M.! loc
        eq = equipment $ sharkCfgs cfgs
        aEs = filter (`elem` allowedEq) $ gameOwnedEquipment myEquip
        lupE et =
            let eqEntry = eq M.! et
                eqSize = equipSize eqEntry
            in (et, eqSize, T.concat [equipText eqEntry, " - ", T.pack (show eqSize), " slots"])
        aEs' = lupE <$> aEs
        usedSlots = sum $ (\s -> equipSize $ eq M.! s) <$> chsn
        openSlots = slots - usedSlots
        slotTxt = T.concat ["Equipment Loaded: ", T.pack (show usedSlots), " slots"]
        words = [ TextDisplay "Select Trip" 50 50 8 White Nothing
                , TextDisplay "Equipment" 150 175 10 White Nothing
                , TextDisplay (T.concat ["Max Slots: ", T.pack (show slots), " slots"]) 200 350 3 Green Nothing
                , TextDisplay slotTxt 200 400 3 Green Nothing
                ]
        opts' = (\(k, s, t) -> SelectOption t k (k `elem` chsn) True $ s > openSlots) <$> aEs'
        update = TripEquipmentSelect gd (idx, loc)
        act = if null chsn  || usedSlots > slots then Nothing else Just $ \ls -> TripReview gd (idx, loc) ls
        back = TripDestinationSelect gd idx


reviewTripMenu :: GameData -> (Int, T.Text) -> [T.Text] -> GameConfigs -> GameMenu
reviewTripMenu gd (idx, loc) eqs cfgs = GameMenu (textView words) (Menu (selOneOpts 400 600 3 5 opts (Just backOpt) (CursorRect White) 0) Nothing)
    where
        region = gameCurrentRegion gd
        boat = gameActiveBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! boat
        trip = tripInfo (sharkCfgs cfgs) region loc boat eqs
        funds = gameDataFunds gd
        tc = tripCost trip
        enoughFunds = funds >= tc
        (fundTxt:tripTxt:afterTxt:_) = splitJustSpacing [ ("Current Funds: ", showMoney funds)
                                                        , ("Trip Cost: ", showMoney tc)
                                                        , ("After Trip: ", showMoney (funds - tc))
                                                        ]
        tripLenTxt = T.append (T.append "Trip Length: " (T.pack (show (tripLength trip)))) " month(s)"
        words = [ TextDisplay "Review Trip" 50 20 8 White Nothing
                , TextDisplay "Details" 150 125 10 White Nothing
                , TextDisplay (T.append "Boat: " (boatName boatInfo)) 300 275 3 White Nothing
                , TextDisplay (T.append "Fuel Cost: $" (T.pack (show (boatFuelCost boatInfo)))) 300 325 3 White Nothing
                -- , TextDisplay tripLenTxt 250 375 3 White Nothing
                , TextDisplay fundTxt 250 425 3 Green Nothing
                , TextDisplay tripTxt 250 475 3 Red Nothing
                , TextDisplay afterTxt 250 525 3 (if enoughFunds then Green else Red) Nothing
                ]
        gd' = gd { gameDataFunds = funds - tc, gameDataMonth = gameDataMonth gd + tripLength trip }
        (gd'', atmpts) = initTripProgress gd' (gameCurrentRegion gd) loc boat eqs cfgs
        progress = TripProgress
        opts = [ MenuAction "Start Trip" Nothing (if enoughFunds then Just (TripProgress gd'' atmpts) else Nothing)
               , MenuAction "Back to equipment" Nothing $ Just (TripEquipmentSelect gd (idx, loc) eqs 0)
               ]
        backOpt = MenuAction "Abort Trip" Nothing $ Just (ResearchCenter gd)

tripProgressMenu :: GameData -> TripState -> GameConfigs -> InputState -> Graphics -> GameView
tripProgressMenu gd tp cfgs (InputState _ _ _ ts) gr =
    case tripTries tp of
        [] -> GameView (v []) Nothing [TimeoutData ts 0 $ TimeoutNext $ TripResults gd tp] Nothing
        (ta@(TripAttempt mn h):tl) ->
            let (gd', sfM) = exec ta
                (gd'', randomDelay) = getRandomRange gd' 800 4000
                tp' = newTrip gd'' sfM tl
                lastText = T.concat ["Using ", getData h equipText]
                nextTimeout = TimeoutData ts (fromIntegral randomDelay) $ TimeoutNext $ SharkFound gd'' sfM tp'
            in GameView (v [TextDisplay lastText 150 280 3 Green Nothing]) Nothing [nextTimeout, animTO] Nothing
    where
        curA = length $ tripTries tp
        allA = tripTotalTries tp
        v w = View ((,0) <$> (words ++ w)) [] [rayAnim] [backRect, progressRect] Nothing
        animTO = TimeoutData ts 150 $ TimeoutAnimation $ startTextAnim gr
        rayAnim = centerAnimation gr 350 5.0 "skate"
        progX = midStartX gr progW
        progY = 650
        progH = 80
        progW = max 1000 $ percentWidth gr 0.8
        backRect = RPlace Gray progX progY progW progH 1
        p = floor (fromIntegral (allA - curA) / fromIntegral allA * 100)
        progressRect = RPlace Green progX progY ((progW `div` 100) * p) progH 2
        words = [ TextDisplay "Trip Progress" 50 50 8 White Nothing
                , TextDisplay "Looking for sharks..." 100 200 4 Blue Nothing
                ]
        newTrip gd'' sfM tl = case sfM of
                            Nothing -> tp { tripTries = tl }
                            Just sf -> tp { tripTries = tl, sharkFinds = sharkFinds tp ++ [sf]}
        exec = executeTrip (sharkCfgs cfgs) gd (trip tp)

sharkFoundMenu :: GameData -> Maybe SharkFind -> TripState -> GameConfigs -> Graphics -> GameMenu
sharkFoundMenu gd sfM tp cfgs gr = GameMenu (View ((,0) <$> words) imgs [] [] Nothing) (Menu (selOneOpts menuOptX (imgEnd + 50) 3 4 opts Nothing (CursorRect White) 0) Nothing)
    where
        menuOptX = graphicsWindowWidth gr - 400
        typeText sf = T.append (T.append "You " infoTypeText) " a "
            where
                infoTypeText = case getData (findEquipment sf) equipInfoType of
                    Caught -> "caught"
                    Observed -> "observed"
        sharkText sf = getData (findSpecies sf) sharkName
        sharkImgKey sf = getData (findSpecies sf) sharkImage
        makeImg yStart = centerScalingImage gr yStart 100 200 1.6
        (words, imgs, imgEnd) = case sfM of
                    Nothing ->
                        let (netImage, xStart, yEnd, _) = makeImg 360 "empty_net"
                        in ([ TextDisplay "No Shark" 50 20 10 White Nothing
                           , TextDisplay "Found" 150 150 8 White Nothing
                           , TextDisplay "Better luck next time!" 200 280 4 White Nothing
                           ], [netImage], yEnd)
                    Just sf ->
                        let (sharkImg, xStart, yEnd, scale) = makeImg 275 $ sharkImgKey sf
                        in ([ TextDisplay (typeText sf) 60 20 8 White Nothing
                            , TextDisplay (sharkText sf) 200 150 5 Blue Nothing
                            ], [sharkImg], yEnd)
        nextState = if null (tripTries tp) then TripResults gd tp else TripProgress gd tp
        opts = [ MenuAction "Continue Trip" Nothing $ Just nextState ]

tripResultsMenu :: GameData -> TripState -> GameConfigs -> Graphics -> GameMenu
tripResultsMenu gd tp cfgs gr = GameMenu (View ((,0) <$> words) [] [] [] scrollVM) (Menu (selOneOpts (width `div` 2) (height - 100) 3 4 [] (Just backOpt) (CursorRect White) 0) Nothing)
    where
        width = graphicsWindowWidth gr
        height = graphicsWindowHeight gr
        sfMap = gameDataFoundSharks gd
        gd' = foldl (\g sf -> addShark g (mkGameShark sf)) gd (sharkFinds tp)
        words = [TextDisplay "Trip Complete!" 50 50 8 White Nothing]
        findText sf = T.concat ["- ", getData (findSpecies sf) sharkName, " ", infoTypeText]
            where
                infoTypeText = case getData (findEquipment sf) equipInfoType of
                    Caught -> "caught"
                    Observed -> "observed"
        findDisplays (i, sf) = [ TextDisplay (findText sf) 130 (250 + (i * 75)) 3 White Nothing
                               -- , TextDisplay (T.append "at " (monthToText (findMonth sf))) 200 (300 + (i * 100)) 3 White
                               ]
        sharkFindsTxt = concatMap findDisplays $ zip [0..] (sharkFinds tp)
        scrollVM = mkScrollView gr sharkFindsTxt [] [] 0 (height - 120) 10
        backOpt = MenuAction "Back to Research Center" Nothing $ Just (ResearchCenter gd')
