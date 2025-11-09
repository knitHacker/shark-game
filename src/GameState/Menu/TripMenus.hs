{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.TripMenus
    ( mapMenu
    , equipmentPickMenu
    , reviewTripMenu
    , tripProgressMenu
    , sharkFoundMenu
    , tripResultsMenu
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
import qualified Data.Text as T
import Data.Maybe (isJust, fromJust)

import Graphics.Types
import Graphics.Menu
import Graphics.TextUtil

import Debug.Trace

mapMenu :: GameData -> GameConfigs -> GameMenu
mapMenu gd cfgs = GameMenu (View words [] [] Nothing) (Menu options Nothing)
    where
        myBoat = gameBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! myBoat
        options = scrollOpts 250 400 4 8 (BasicSOALOpts (OALOpts opts mc)) [rtOpt] 4 0
        locs = (\(loc, lCfg) -> (loc, showText lCfg)) <$> M.assocs (siteLocations $ sharkCfgs cfgs)
        mc = CursorRect White
        words = [ TextDisplay "Select Trip" 50 50 8 White
                , TextDisplay "Destination" 150 200 10 White
                ]
        opts = mkOptEntry <$> locs
        rtOpt = MenuAction "Return to Lab" $ Just $ ResearchCenter gd
        mkOptEntry (loc, txt) = MenuAction txt $ if loc `elem` boatReachableBiomes boatInfo then Just (TripEquipmentSelect gd loc [] 0) else Nothing

-- assuming I add ability to have more than one boat, need to select boat in new menu

equipmentPickMenu :: GameData -> T.Text -> [T.Text] -> Int -> GameConfigs -> GameMenu
equipmentPickMenu gd loc chsn pos cfgs = GameMenu (View words [] [] Nothing) (Menu (selMultOpts 250 475 3 8 opts' update act (Just back) pos) Nothing)
    where
        myEquip = gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! gameBoat myEquip
        slots = boatEquipmentSlots boatInfo
        allowedEq = allowedEquipment $ siteLocations (sharkCfgs cfgs) M.! loc
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
        words = [ TextDisplay "Select Trip" 50 50 8 White
                , TextDisplay "Equipment" 150 175 10 White
                , TextDisplay (T.concat ["Max Slots: ", T.pack (show slots), " slots"]) 200 350 3 Green
                , TextDisplay slotTxt 200 400 3 Green
                ]
        opts' = (\(k, s, t) -> SelectOption t k (k `elem` chsn) True $ s > openSlots) <$> aEs'
        update = TripEquipmentSelect gd loc
        act = if null chsn  || usedSlots > slots then Nothing else Just $ \ls -> TripReview gd loc ls
        back = TripDestinationSelect gd


reviewTripMenu :: GameData -> T.Text -> [T.Text] -> GameConfigs -> GameMenu
reviewTripMenu gd loc eqs cfgs = GameMenu (View words [] [] Nothing) (Menu (selOneOpts 400 600 3 5 opts (CursorRect White) 0) Nothing)
    where
        boat = gameBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! boat
        trip = tripInfo (sharkCfgs cfgs) loc boat eqs
        funds = gameDataFunds gd
        tc = tripCost trip
        enoughFunds = funds >= tc
        (fundTxt:tripTxt:afterTxt:_) = splitJustSpacing [ ("Current Funds: ", showMoney funds)
                                                        , ("Trip Cost: ", showMoney tc)
                                                        , ("After Trip: ", showMoney (funds - tc))
                                                        ]
        tripLenTxt = T.append (T.append "Trip Length: " (T.pack (show (tripLength trip)))) " month(s)"
        words = [ TextDisplay "Review Trip" 50 20 8 White
                , TextDisplay "Details" 150 125 10 White
                , TextDisplay (T.append "Boat: " (boatName boatInfo)) 300 275 3 White
                , TextDisplay (T.append "Fuel Cost: $" (T.pack (show (boatFuelCost boatInfo)))) 300 325 3 White
                -- , TextDisplay tripLenTxt 250 375 3 White
                , TextDisplay fundTxt 250 425 3 Green
                , TextDisplay tripTxt 250 475 3 Red
                , TextDisplay afterTxt 250 525 3 (if enoughFunds then Green else Red)
                ]
        gd' = gd { gameDataFunds = funds - tc, gameDataMonth = gameDataMonth gd + tripLength trip }
        (gd'', atmpts) = initTripProgress gd' loc boat eqs cfgs
        progress = TripProgress
        opts = [ MenuAction "Start Trip" (if enoughFunds then Just (TripProgress gd'' atmpts) else Nothing)
               , MenuAction "Back to equipment" $ Just (TripEquipmentSelect gd loc eqs 0)
               , MenuAction "Abort Trip" $ Just (ResearchCenter gd)
               ]

tripProgressMenu :: GameData -> TripState -> GameConfigs -> InputState -> GameView
tripProgressMenu gd tp cfgs (InputState _ _ ts) =
    case tripTries tp of
        [] -> GameView (v []) Nothing (Just (TimeoutData ts 0 $ TimeoutNext $ TripResults gd tp)) Nothing
        (ta@(TripAttempt mn h):tl) ->
            let (gd', sfM) = exec ta
                tp' = newTrip sfM tl
                lastText = T.concat ["Using ", getData h equipText]
            in GameView (v [TextDisplay lastText 150 400 3 Green]) Nothing (Just (TimeoutData ts 2000 $ TimeoutNext $ SharkFound gd' sfM tp')) Nothing
    where
        curA = length $ tripTries tp
        allA = tripTotalTries tp
        v w = View (words ++ w) [] [backRect, progressRect] Nothing
        progX = 100
        progY = 500
        progH = 100
        progW = 1000
        backRect = (Gray, progX, progY, progW, progH)
        p = floor (fromIntegral (allA - curA) / fromIntegral allA * 100)
        progressRect = (Green, progX, progY, (progW `div` 100) * p, progH)
        words = [ TextDisplay "Trip Progress" 50 50 8 White
                , TextDisplay "Looking for sharks..." 100 300 4 Blue
                ]
        newTrip sfM tl = case sfM of
                            Nothing -> tp { tripTries = tl }
                            Just sf -> tp { tripTries = tl, sharkFinds = sharkFinds tp ++ [sf]}
        exec = executeTrip (sharkCfgs cfgs) gd (trip tp)

sharkFoundMenu :: GameData -> Maybe SharkFind -> TripState -> GameConfigs -> GameMenu
sharkFoundMenu gd sfM tp cfgs = GameMenu (View words imgs [] Nothing) (Menu (selOneOpts 400 720 3 4 opts (CursorRect White) 0) Nothing)
    where
        typeText sf = T.append (T.append "You " (getData (findEquipment sf) equipInfoType)) " a "
        sharkText sf = getData (findSpecies sf) sharkName
        sharkImg sf = getData (findSpecies sf) sharkImage
        (words, imgs) = case sfM of
                    Nothing -> ([ TextDisplay "No Shark" 50 20 10 White
                               , TextDisplay "Found" 150 150 10 White
                               , TextDisplay "Better luck next time!" 80 300 4 White
                               ], [(375, 350, 1.75, "empty_net")])
                    Just sf -> ([ TextDisplay (typeText sf) 60 20 8 White
                               , TextDisplay (sharkText sf) 150 175 5 Blue
                               ], [(350, 300, 1.75, sharkImg sf)])
        nextState = if null (tripTries tp) then TripResults gd tp else TripProgress gd tp
        opts = [ MenuAction "Continue Trip" $ Just nextState ]

tripResultsMenu :: GameData -> TripState -> GameConfigs -> Graphics -> GameMenu
tripResultsMenu gd tp cfgs gr = GameMenu (View words [] [] scrollVM) (Menu (selOneOpts 200 650 3 4 opts (CursorRect White) 0) Nothing)
    where
        sfMap = gameDataFoundSharks gd
        gd' = foldl (\g sf -> addShark g (mkGameShark sf)) gd (sharkFinds tp)
        words = [TextDisplay "Trip Complete!" 50 50 8 White]
        findText sf = T.concat ["- ", getData (findSpecies sf) sharkName, " ", getData (findEquipment sf) equipInfoType]
        findDisplays (i, sf) = [ TextDisplay (findText sf) 130 (250 + (i * 75)) 3 White
                               -- , TextDisplay (T.append "at " (monthToText (findMonth sf))) 200 (300 + (i * 100)) 3 White
                               ]
        sharkFindsTxt = concatMap findDisplays $ zip [0..] (sharkFinds tp)
        scrollVM = mkScrollView gr sharkFindsTxt [] 0 500 5
        opts = [ MenuAction "Back to Research Center" $ Just (ResearchCenter gd') ]
