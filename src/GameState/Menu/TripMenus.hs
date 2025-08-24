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

import Graphics.Types
import Graphics.Menu
import Graphics.TextUtil

import Debug.Trace

mapMenu :: GameData -> GameConfigs -> Menu GamePlayState
mapMenu gd cfgs = mkMenu words [] Nothing options
    where
        options = scrollOpts 15 120 4 8 (BasicSOALOpts (OALOpts opts mc)) [rtOpt] 4 0
        locs = (\(loc, lCfg) -> (loc, showText lCfg)) <$> M.assocs (siteLocations $ sharkCfgs cfgs)
        mc = CursorRect White
        words = [ TextDisplay "Select Trip" 10 10 8 White
                , TextDisplay "Destination" 15 55 10 White
                ]
        opts = (\(loc, txt) -> MenuAction txt True (TripEquipmentSelect gd loc [] 0)) <$> locs
        rtOpt = MenuAction "Return to Lab" True $ ResearchCenter gd

equipmentPickMenu :: GameData -> T.Text -> [T.Text] -> Int -> GameConfigs -> Menu GamePlayState
equipmentPickMenu gd loc chsn pos cfgs = mkMenu words [] Nothing (selMultOpts 15 130 3 6 opts' update act (Just back) pos)
    where
        locO = siteLocations (sharkCfgs cfgs) M.! loc
        eq = equipment $ sharkCfgs cfgs
        rEs = requiredEquipment locO
        aEs = allowedEquipment locO
        lupE et =
            let eqEntry = eq M.! et
            in (et, T.append (T.append (text eqEntry) " - $") (T.pack (show (price eqEntry))))
        rEs' = lupE <$> rEs
        aEs' = lupE <$> aEs
        cost = foldl (\s opt -> if selectSelected opt then s + price (eq M.! selectKey opt) else s) 0 opts'
        costTxt = T.append "Trip Current Cost: $" (T.pack (show cost))
        words = [ TextDisplay "Select Trip" 10 10 8 White
                , TextDisplay "Equipment" 15 55 10 White
                , TextDisplay costTxt 20 100 4 Green
                ]
        rOpts = (\(k, t) -> SelectOption t k True False) <$> rEs'
        aOpts = (\(k, t) -> SelectOption t k (k `elem` chsn) True) <$> aEs'
        opts' = rOpts ++ aOpts
        update = TripEquipmentSelect gd loc
        act = TripReview gd loc
        back = TripDestinationSelect gd


reviewTripMenu :: GameData -> T.Text -> [T.Text] -> GameConfigs -> Menu GamePlayState
reviewTripMenu gd loc eqs cfgs = mkMenu words [] Nothing (selOneOpts 80 185 3 4 opts (CursorRect White) 0)
    where
        trip = tripInfo (sharkCfgs cfgs) loc eqs
        funds = gameDataFunds gd
        tc = tripCost trip
        enoughFunds = funds >= tc
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        tripTxt = T.append "Trip Cost: $" (T.pack (show tc))
        afterTxt = T.append "After Trip: $" (T.pack (show (funds - tc)))
        tripLenTxt = T.append (T.append "Trip Length: " (T.pack (show (tripLength trip)))) " month(s)"
        words = [ TextDisplay "Review Trip" 10 10 8 White
                , TextDisplay "Details" 15 55 10 White
                , TextDisplay fundTxt 25 105 3 Green
                , TextDisplay tripTxt 25 125 3 Red
                , TextDisplay afterTxt 25 145 3 (if enoughFunds then Green else Red)
                , TextDisplay tripLenTxt 25 165 3 White
                ]
        gd' = gd { gameDataFunds = funds - tc, gameDataMonth = gameDataMonth gd + tripLength trip }
        (gd'', atmpts) = initTripProgress gd' loc eqs cfgs
        progress = TripProgress
        opts = [ MenuAction "Start Trip" enoughFunds (if enoughFunds then TripProgress gd'' atmpts else TripReview gd loc eqs)
               , MenuAction "Back to equipment" True (TripEquipmentSelect gd loc eqs 0)
               , MenuAction "Abort Trip" True (ResearchCenter gd)
               ]

tripProgressMenu :: GameData -> TripState -> GameConfigs -> InputState -> TimeoutView GamePlayState
tripProgressMenu gd tp cfgs (InputState _ _ ts) =
    case tripTries tp of
        [] -> TimeoutView ts 0 (v []) $ TripResults gd tp
        (ta@(TripAttempt mn h):tl) ->
            let (gd', sfM) = exec ta
                tp' = newTrip sfM tl
                lastText = T.concat ["month ", T.pack (show mn), ", using ", getData h text]
            in TimeoutView ts 1 (v [TextDisplay lastText 30 60 4 Green]) $ SharkFound gd' sfM tp'
    where
        curA = length $ tripTries tp
        allA = tripTotalTries tp
        v w = View (words ++ w) [] [backRect, progressRect] Nothing
        progX = 30
        progY = 150
        progH = 20
        backRect = (Gray, progX, progY, 200, progH)
        p = floor (fromIntegral (allA - curA) / fromIntegral allA * 100)
        progressRect = (Green, progX, progY, 2*p, progH)
        words = [ TextDisplay "Trip Progress" 10 10 8 White
                , TextDisplay "Looking for sharks..." 20 100 5 Blue
                ]
        newTrip sfM tl = case sfM of
                            Nothing -> tp { tripTries = tl }
                            Just sf -> tp { tripTries = tl, sharkFinds = sharkFinds tp ++ [sf]}
        exec = executeTrip (sharkCfgs cfgs) gd (trip tp)

sharkFoundMenu :: GameData -> Maybe SharkFind -> TripState -> GameConfigs -> Menu GamePlayState
sharkFoundMenu gd sfM tp cfgs = mkMenu words imgs Nothing (selOneOpts 80 220 3 4 opts (CursorRect White) 0)
    where
        typeText sf = T.append (T.append "You " (getData (findEquipment sf) infoType)) " a "
        sharkText sf = getData (findSpecies sf) sharkName
        sharkImg sf = getData (findSpecies sf) sharkImage
        (words, imgs) = case sfM of
                    Nothing -> ([ TextDisplay "No Shark" 40 10 10 White
                               , TextDisplay "Found" 70 50 10 White
                               , TextDisplay "Better luck next time!" 20 100 5 White
                               ], [(70, 115, 0.5, "empty_net")])
                    Just sf -> ([ TextDisplay (typeText sf) 10 10 8 White
                               , TextDisplay (sharkText sf) 20 70 5 Blue
                               ], [(70, 100, 0.5, sharkImg sf)])
        nextState = if null (tripTries tp) then TripResults gd tp else TripProgress gd tp
        opts = [ MenuAction "Continue Trip" True nextState ]

tripResultsMenu :: GameData -> TripState -> GameConfigs -> Menu GamePlayState
tripResultsMenu gd tp cfgs = mkMenu words [] scrollVM (selOneOpts 60 200 3 4 opts (CursorRect White) 0)
    where
        sfMap = gameDataFoundSharks gd
        gd' = foldl (\g sf -> addShark g (mkGameShark sf)) gd (sharkFinds tp)
        words = [TextDisplay "Trip Complete!" 10 10 8 White]
        findText sf = T.concat [getData (findSpecies sf) sharkName, " ", getData (findEquipment sf) infoType]
        findDisplays (i, sf) = [ TextDisplay (findText sf) 30 (50 + (i * 40)) 3 White
                               , TextDisplay (T.append "at " (monthToText (findMonth sf))) 50 (65 + (i * 40)) 3 White
                               ]
        sharkFindsTxt = concatMap findDisplays $ zip [0..] (sharkFinds tp)
        scrollVM = if length sharkFindsTxt > 0 then Just (mkScrollView sharkFindsTxt [] 190 0) else Nothing
        opts = [ MenuAction "Back to Research Center" True (ResearchCenter gd') ]

