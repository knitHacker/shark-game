{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.TripMenus
    ( TripDestinationSelectState(..)
    , initialTripDestinationSelect
    , TripEquipmentSelectState(..)
    , TripReviewState(..)
    , TripProgressState(..)
    , SharkFoundState(..)
    , TripResultsState(..)
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Int (Int64)

import Configs
import SaveData
import Util
import OutputHandles.Types ( Color(..), TextDisplay(TextDisplay) )
import GameState.Types
import GameState.Helpers
import Graphics.Types hiding (updateMenu)
import Graphics.Menu
import Graphics.TextUtil
import Graphics.ImageUtil
import Graphics.Animation
import InputState
import Shark.Types
import Shark.Trip
import Shark.Util

-- | Extract current scroll offset from a scroll-list MenuData.
menuScrollOffset :: MenuData a -> Int
menuScrollOffset (MenuData (ScrollListOpts (SLOpts _ _ _ (Scroll _ off))) _ _) = off
menuScrollOffset _ = 0


-- ===========================================================================
-- TripDestinationSelectState
-- ===========================================================================

data TripDestinationSelectState = TripDestinationSelectState
    { tdsGameData :: !GameData
    , tdsScroll   :: !ScrollMenuState   -- cursor + scroll offset
    }

mkDestMenu :: GameData -> ScrollMenuState -> Graphics -> GameConfigs -> ViewMenu
mkDestMenu gd scrollState gr cfgs = ViewMenu $ Menu options Nothing
    where
        region   = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
        myBoat   = gameActiveBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) M.! myBoat
        allLocs  = M.assocs (getData region siteLocations)
        locs     = (\(idx, (loc, lCfg)) -> (idx, loc, showText lCfg)) <$> zip [0..] allLocs
        rtOpt    = MenuAction "Return to Lab" Nothing $ Just $ PureStep $ MoveTo $ ResearchCenter gd
        mkOptEntry (idx, loc, txt) =
            MenuAction txt Nothing $
                if loc `elem` boatReachableBiomes boatInfo
                then Just $ PureStep $ Transition $
                         AnyGamePlayState (TripEquipmentSelectState gd (idx, loc) [] 0) False
                else Nothing
        opts    = mkOptEntry <$> locs
        options = resizingScrollOpts gr 50 140 300 3 8
                      (OALOpts opts Nothing Nothing (CursorRect White)) (Just rtOpt) [] scrollState

instance GamePlayState TripDestinationSelectState where
    think s@(TripDestinationSelectState gd scrollState) cfgs inputRes gr =
        let inputs     = newInputs inputRes
            ViewMenu m = mkDestMenu gd scrollState gr cfgs
        in thinkWithPause gd inputRes $ case updateMenu inputs m of
            Just (Right upd) -> upd
            Just (Left m')   ->
                let newScroll = ScrollMenuState (cursorPosition (options m'))
                                               (menuScrollOffset (options m'))
                in PureStep $ Refresh $ AnyGamePlayState (s { tdsScroll = newScroll }) True
            Nothing
                | windowResized inputRes /= Nothing ->
                    PureStep $ Refresh $ AnyGamePlayState s True
                | otherwise -> PureStep UseCache

    draw (TripDestinationSelectState gd scrollState@(ScrollMenuState cursor _)) gr cfgs =
        let region         = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            allLocs        = M.assocs (getData region siteLocations)
            selectedLocCfg = if cursor >= 0 && cursor < length allLocs
                             then snd (allLocs !! cursor)
                             else snd (head allLocs)
            (wrappedDesc, _) = wrapText gr (biomeDescription selectedLocCfg) 800 300 400 10 2 White
            words          = [ TextDisplay "Select Trip"  40 40  7 White Nothing
                             , TextDisplay "Destination"  80 160 8 White Nothing
                             ] ++ wrappedDesc
        in GameView (textView words) Nothing (Just $ mkDestMenu gd scrollState gr cfgs)


initialTripDestinationSelect :: GameData -> AnyGamePlayState
initialTripDestinationSelect gd = AnyGamePlayState (TripDestinationSelectState gd (ScrollMenuState 0 0)) False


-- ===========================================================================
-- TripEquipmentSelectState
-- ===========================================================================

data TripEquipmentSelectState = TripEquipmentSelectState
    { tesGameData :: !GameData
    , tesLoc      :: !(Int, T.Text)
    , tesSelected :: ![T.Text]
    , tesCursor   :: !Int
    }

mkEquipMenu :: TripEquipmentSelectState -> Graphics -> GameConfigs -> ViewMenu
mkEquipMenu s@(TripEquipmentSelectState gd loc@(idx, locKey) selected _) gr cfgs = ViewMenu $ Menu options Nothing
    where
        region    = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
        myEquip   = gameDataEquipment gd
        boatInfo  = boats (sharkCfgs cfgs) M.! gameActiveBoat myEquip
        slots     = boatEquipmentSlots boatInfo
        eq        = equipment $ sharkCfgs cfgs
        allowedEq = allowedEquipment $ getData region siteLocations M.! locKey
        aEs       = filter (`elem` allowedEq) $ gameOwnedEquipment myEquip
        lupE et   = let eqEntry = eq M.! et
                        eqSize  = equipSize eqEntry
                    in (et, eqSize, T.concat [equipText eqEntry, " - ", T.pack (show eqSize), " slots"])
        aEs'      = lupE <$> aEs
        usedSlots = sum $ (\e -> equipSize $ eq M.! e) <$> selected
        openSlots = slots - usedSlots
        opts'     = (\(k, sz, t) -> SelectOption t k (k `elem` selected) True (sz > openSlots)) <$> aEs'
        update    = \sel newPos -> PureStep $ Refresh $
                        AnyGamePlayState (s { tesSelected = sel, tesCursor = newPos }) True
        actM      = if null selected || usedSlots > slots
                    then Nothing
                    else Just $ \sel -> PureStep $ Transition $
                             AnyGamePlayState (TripReviewState gd loc sel 0) False
        backAct   = Just $ PureStep $ Transition $
                        AnyGamePlayState (TripDestinationSelectState gd (ScrollMenuState idx 0)) False
        options   = selMultOpts 250 475 3 8 opts' update actM backAct (tesCursor s)

instance GamePlayState TripEquipmentSelectState where
    think s@(TripEquipmentSelectState gd _ _ _) cfgs inputRes gr =
        let inputs     = newInputs inputRes
            ViewMenu m = mkEquipMenu s gr cfgs
        in thinkWithPause gd inputRes $ case updateMenu inputs m of
            Just (Right upd) -> upd
            Just (Left m')   ->
                PureStep $ Refresh $ AnyGamePlayState (s { tesCursor = cursorPosition (options m') }) True
            Nothing
                | windowResized inputRes /= Nothing ->
                    PureStep $ Refresh $ AnyGamePlayState s True
                | otherwise -> PureStep UseCache

    draw s@(TripEquipmentSelectState gd (_, locKey) selected _) gr cfgs =
        let region    = getEntry (regions (sharkCfgs cfgs)) (gameCurrentRegion gd)
            myEquip   = gameDataEquipment gd
            boatInfo  = boats (sharkCfgs cfgs) M.! gameActiveBoat myEquip
            slots     = boatEquipmentSlots boatInfo
            eq        = equipment $ sharkCfgs cfgs
            usedSlots = sum $ (\e -> equipSize $ eq M.! e) <$> selected
            slotTxt   = T.concat ["Equipment Loaded: ", T.pack (show usedSlots), " slots"]
            words     = [ TextDisplay "Select Trip"     50  50  8 White Nothing
                        , TextDisplay "Equipment"       150 175 10 White Nothing
                        , TextDisplay (T.concat ["Max Slots: ", T.pack (show slots), " slots"]) 200 350 3 Green Nothing
                        , TextDisplay slotTxt           200 400 3 Green Nothing
                        ]
        in GameView (textView words) Nothing (Just $ mkEquipMenu s gr cfgs)


-- ===========================================================================
-- TripReviewState
-- ===========================================================================

data TripReviewState = TripReviewState
    { trsGameData :: !GameData
    , trsLoc      :: !(Int, T.Text)
    , trsSelected :: ![T.Text]
    , trsCursor   :: !Int
    }

mkReviewMenu :: TripReviewState -> Graphics -> GameConfigs -> ViewMenu
mkReviewMenu s@(TripReviewState gd loc@(idx, locKey) eqs _) gr cfgs = ViewMenu $ Menu options Nothing
    where
        boat        = gameActiveBoat $ gameDataEquipment gd
        boatInfo    = boats (sharkCfgs cfgs) M.! boat
        trip        = tripInfo (sharkCfgs cfgs) (gameCurrentRegion gd) locKey boat eqs
        funds       = gameDataFunds gd
        tc          = tripCost trip
        enoughFunds = funds >= tc
        gd'         = gd { gameDataFunds = funds - tc
                         , gameDataMonth = gameDataMonth gd + tripLength trip }
        (gd'', tripState) = initTripProgress gd' (gameCurrentRegion gd) locKey boat eqs cfgs
        startOpt    = MenuAction "Start Trip" Nothing $
            if enoughFunds
            then Just $ PureStep $ Transition $ AnyGamePlayState (TripProgressState gd'' tripState 0 0 0 0) False
            else Nothing
        backEquipOpt = MenuAction "Back to equipment" Nothing $ Just $ PureStep $ Transition $
            AnyGamePlayState (TripEquipmentSelectState gd loc eqs 0) False
        backOpt     = MenuAction "Abort Trip" Nothing $ Just $ PureStep $ Transition $
            AnyGamePlayState (TripDestinationSelectState gd (ScrollMenuState idx 0)) False
        options     = selOneOpts 400 600 3 5 [startOpt, backEquipOpt] (Just backOpt) (CursorRect White) (trsCursor s)

instance GamePlayState TripReviewState where
    think s@(TripReviewState gd _ _ _) cfgs inputRes gr =
        let inputs     = newInputs inputRes
            ViewMenu m = mkReviewMenu s gr cfgs
        in thinkWithPause gd inputRes $ case updateMenu inputs m of
            Just (Right upd) -> upd
            Just (Left m')   ->
                PureStep $ Refresh $ AnyGamePlayState (s { trsCursor = cursorPosition (options m') }) True
            Nothing
                | windowResized inputRes /= Nothing ->
                    PureStep $ Refresh $ AnyGamePlayState s True
                | otherwise -> PureStep UseCache

    draw s@(TripReviewState gd (_, locKey) eqs _) gr cfgs =
        let boat        = gameActiveBoat $ gameDataEquipment gd
            boatInfo    = boats (sharkCfgs cfgs) M.! boat
            trip        = tripInfo (sharkCfgs cfgs) (gameCurrentRegion gd) locKey boat eqs
            funds       = gameDataFunds gd
            tc          = tripCost trip
            enoughFunds = funds >= tc
            (fundTxt:tripTxt:afterTxt:_) = splitJustSpacing
                [ ("Current Funds: ", showMoney funds)
                , ("Trip Cost: ",     showMoney tc)
                , ("After Trip: ",    showMoney (funds - tc))
                ]
            words = [ TextDisplay "Review Trip"  50  20  8 White Nothing
                    , TextDisplay "Details"      150 125 10 White Nothing
                    , TextDisplay (T.append "Boat: "       (boatName boatInfo)) 300 275 3 White Nothing
                    , TextDisplay (T.append "Fuel Cost: $" (T.pack (show (boatFuelCost boatInfo)))) 300 325 3 White Nothing
                    , TextDisplay fundTxt        250 425 3 Green Nothing
                    , TextDisplay tripTxt        250 475 3 Red Nothing
                    , TextDisplay afterTxt       250 525 3 (if enoughFunds then Green else Red) Nothing
                    ]
        in GameView (textView words) Nothing (Just $ mkReviewMenu s gr cfgs)


-- ===========================================================================
-- Shared helpers
-- ===========================================================================

sharkFindToData :: SharkFind -> GameSharkData
sharkFindToData sf = GameShark
    { gameSharkMonth     = findMonth sf
    , gameSharkSpecies   = entryKey (findSpecies sf)
    , gameSharkRegion    = fst (entryKey (findLocation sf))
    , gameSharkLocation  = snd (entryKey (findLocation sf))
    , gameSharkEquipment = entryKey (findEquipment sf)
    }

-- | Transition to next progress screen or to results if all attempts are done.
nextProgressOrResults :: GameData -> TripState -> AnyGamePlayState
nextProgressOrResults gd ts
    | null (tripTries ts) = AnyGamePlayState (TripResultsState gd ts 0) False
    | otherwise           = AnyGamePlayState (TripProgressState gd ts 0 0 0 0) False


-- ===========================================================================
-- TripProgressState
-- ===========================================================================

tripAnimPeriod :: Int64
tripAnimPeriod = 150

data TripProgressState = TripProgressState
    { tpsGameData  :: !GameData
    , tpsTripState :: !TripState
    , tpsAnimFrame :: !Int
    , tpsLastAnim  :: !Int64
    , tpsStartTime :: !Int64   -- 0 = not yet initialised
    , tpsDuration  :: !Int64   -- 0 = not yet set; randomised on first think
    }

instance GamePlayState TripProgressState where
    think s@(TripProgressState gd ts animFrame lastAnim startTime0 duration0) cfgs inputRes gr =
        let inputs    = newInputs inputRes
            ts_now    = timestamp inputs
            -- Initialise random duration and start time on first think
            (gd', duration) =
                if duration0 == 0
                then let (gd'', d) = getRandomRange gd 800 4000
                     in (gd'', fromIntegral d)
                else (gd, duration0)
            startTime = if startTime0 == 0 then ts_now else startTime0
            -- Animation
            animTick   = ts_now - lastAnim >= tripAnimPeriod
            maxFrame   = animFrameCount $ graphicsAnimTextures gr M.! "skate"
            animFrame' = if animTick then (animFrame + 1) `mod` maxFrame else animFrame
            lastAnim'  = if animTick then ts_now else lastAnim
            s' = s { tpsGameData  = gd'
                   , tpsAnimFrame = animFrame'
                   , tpsLastAnim  = lastAnim'
                   , tpsStartTime = startTime
                   , tpsDuration  = duration }
            timedOut = ts_now - startTime >= duration
        in if not timedOut
           then if animTick || windowResized inputRes /= Nothing || startTime0 == 0
                then PureStep $ Refresh $ AnyGamePlayState s' True
                else PureStep UseCache
           else case tripTries ts of
               [] -> PureStep $ Transition $ AnyGamePlayState (TripResultsState gd' ts 0) False
               (attempt:rest) ->
                   let (gd'', findM) = executeTrip (sharkCfgs cfgs) gd' (trip ts) attempt
                       ts' = case findM of
                           Nothing -> ts { tripTries = rest }
                           Just sf -> ts { tripTries = rest, sharkFinds = sharkFinds ts ++ [sf] }
                   in PureStep $ Transition $ AnyGamePlayState (SharkFoundState gd'' ts' findM 0) False

    draw (TripProgressState gd ts animFrame _ _ _) gr _ =
        let skateAnim  = centerAnimation gr 350 5.0 "skate"
            skate'     = skateAnim { animFrame = animFrame }
            curA       = length (tripTries ts)
            allA       = tripTotalTries ts
            progW      = max 1000 $ percentWidth gr 0.8
            progX      = midStartX gr progW
            progY      = 650
            progH      = 80
            p          = floor (fromIntegral (allA - curA) / fromIntegral allA * (100 :: Double))
            backRect   = RPlace Gray  progX progY progW               progH 1
            fillRect   = RPlace Green progX progY ((progW `div` 100) * p) progH 2
            eqTxt      = case tripTries ts of
                (attempt:_) -> T.append "Using " $ getData (attemptEqipment attempt) equipText
                []          -> "Finishing up..."
            words = [ TextDisplay "Trip Progress"        50  50  8 White Nothing
                    , TextDisplay "Looking for sharks..." 100 200 4 Blue  Nothing
                    , TextDisplay eqTxt                  150 280 3 Green Nothing
                    ]
            v = View ((,0) <$> words) [] [skate'] [backRect, fillRect] Nothing
        in GameView v Nothing Nothing


-- ===========================================================================
-- SharkFoundState
-- ===========================================================================

data SharkFoundState = SharkFoundState
    { sfsGameData  :: !GameData
    , sfsTripState :: !TripState
    , sfsFind      :: !(Maybe SharkFind)
    , sfsCursor    :: !Int
    }

mkSharkFoundMenu :: Int -> GameData -> TripState -> Int64 -> ViewMenu
mkSharkFoundMenu cursor gd ts ts_now = ViewMenu $ Menu options Nothing
    where
        next    = nextProgressOrResults gd ts
        options = selOneOpts menuOptX 750 3 4
                      [MenuAction "Continue Trip" Nothing (Just $ PureStep $ Transition next)]
                      Nothing (CursorRect White) cursor
        menuOptX = 500  -- will be centred by selOneOpts x param

instance GamePlayState SharkFoundState where
    think s@(SharkFoundState gd ts _ _) _ inputRes _ =
        let inputs     = newInputs inputRes
            ts_now     = timestamp inputs
            ViewMenu m = mkSharkFoundMenu (sfsCursor s) gd ts ts_now
        in case updateMenu inputs m of
            Just (Right upd) -> upd
            Just (Left m')   ->
                PureStep $ Refresh $ AnyGamePlayState (s { sfsCursor = cursorPosition (options m') }) True
            Nothing
                | windowResized inputRes /= Nothing -> PureStep $ Refresh $ AnyGamePlayState s True
                | otherwise -> PureStep UseCache

    draw s@(SharkFoundState gd ts sfM _) gr _ =
        let menuOptX           = graphicsWindowWidth gr - 400
            makeImg yStart key = centerScalingImage gr yStart 100 200 1.6 key
            (words, imgs, _)   = case sfM of
                Nothing ->
                    let (netImg, _, yEnd, _) = makeImg 360 "empty_net"
                    in ( [ TextDisplay "No Shark"              50  20  10 White Nothing
                         , TextDisplay "Found"                 150 150  8 White Nothing
                         , TextDisplay "Better luck next time!" 200 280  4 White Nothing
                         ]
                       , [netImg], yEnd )
                Just sf ->
                    let typeText = case getData (findEquipment sf) equipInfoType of
                            Caught   -> "You caught a"
                            Observed -> "You observed a"
                        (sharkImg, _, yEnd, _) = makeImg 275 (getData (findSpecies sf) sharkImage)
                    in ( [ TextDisplay typeText                              60  20  8 White Nothing
                         , TextDisplay (getData (findSpecies sf) sharkName) 200 150  5 Blue  Nothing
                         ]
                       , [sharkImg], yEnd )
            v = View ((,0) <$> words) imgs [] [] Nothing
        in GameView v Nothing (Just $ mkSharkFoundMenu (sfsCursor s) gd ts 0)


-- ===========================================================================
-- TripResultsState
-- ===========================================================================

data TripResultsState = TripResultsState
    { tresGameData  :: !GameData
    , tresTripState :: !TripState
    , tresCursor    :: !Int
    }

mkTripResultsMenu :: Int -> GameData -> TripState -> ViewMenu
mkTripResultsMenu cursor gd ts = ViewMenu $ Menu options Nothing
    where
        -- Add all found sharks to game data when returning to lab
        gd' = foldl (\g sf -> addShark g (sharkFindToData sf)) gd (sharkFinds ts)
        options = selOneOpts 500 750 3 4
                      [MenuAction "Back to Research Center" Nothing
                           (Just $ SaveFile gd' $ MoveTo $ ResearchCenter gd')]
                      Nothing (CursorRect White) cursor

instance GamePlayState TripResultsState where
    think s@(TripResultsState gd ts _) _ inputRes _ =
        let inputs     = newInputs inputRes
            ViewMenu m = mkTripResultsMenu (tresCursor s) gd ts
        in case updateMenu inputs m of
            Just (Right upd) -> upd
            Just (Left m')   ->
                PureStep $ Refresh $ AnyGamePlayState (s { tresCursor = cursorPosition (options m') }) True
            Nothing
                | windowResized inputRes /= Nothing -> PureStep $ Refresh $ AnyGamePlayState s True
                | otherwise -> PureStep UseCache

    draw s@(TripResultsState gd ts _) gr _ =
        let finds    = sharkFinds ts
            height   = graphicsWindowHeight gr
            findText sf = T.concat
                [ "- ", getData (findSpecies sf) sharkName, " "
                , case getData (findEquipment sf) equipInfoType of
                    Caught   -> "caught"
                    Observed -> "observed"
                ]
            findDisplays = [ TextDisplay (findText sf) 130 (fromIntegral (250 + i * 75)) 3 White Nothing
                           | (i, sf) <- zip [0 :: Int ..] finds ]
            scrollVM = mkScrollView gr findDisplays [] [] 0 (height - 120) 10
            words    = [TextDisplay "Trip Complete!" 50 50 8 White Nothing]
            v = View ((,0) <$> words) [] [] [] scrollVM
        in GameView v Nothing (Just $ mkTripResultsMenu (tresCursor s) gd ts)
