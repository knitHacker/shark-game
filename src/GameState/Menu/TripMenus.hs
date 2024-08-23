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

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Shark.Trip

import Debug.Trace

mapMenu :: GameData -> GameConfigs -> Menu
mapMenu gd cfgs = mkMenu words [] (selOneOpts 15 140 4 8 opts' mc) 0
    where
        locs = (\(loc, lCfg) -> (loc, showText lCfg)) <$> (M.assocs $ siteLocations $ sharkCfgs cfgs)
        mc = CursorRect White
        words = [ TextDisplay "Select Trip" 10 10 8 White
                , TextDisplay "Destination" 15 55 10 White
                ]
        opts = (\(loc, txt) -> MenuAction txt (TripEquipmentSelect gd loc [] 0)) <$> locs
        rtOpt = MenuAction "Return to Lab" $ ResearchCenter gd
        opts' = opts ++ [rtOpt]

equipmentPickMenu :: GameData -> T.Text -> [T.Text] -> Int -> GameConfigs -> Menu
equipmentPickMenu gd loc chsn pos cfgs = mkMenu words [] (selMultOpts 15 130 3 6 opts' update act (Just back)) pos
    where
        locO = (siteLocations (sharkCfgs cfgs)) M.! loc
        eq = equipment $ sharkCfgs cfgs
        rEs = requiredEquipment locO
        aEs = allowedEquipment locO
        lupE et =
            let eqEntry = eq M.! et
            in (et, T.append (T.append (text eqEntry) " - $") (T.pack (show (price eqEntry))))
        rEs' = lupE <$> rEs
        aEs' = lupE <$> aEs
        cost = foldl (\s opt -> if selectSelected opt then s + price (eq M.! (selectKey opt)) else s) 0 opts'
        costTxt = T.append "Trip Current Cost: $" (T.pack (show cost))
        words = [ TextDisplay "Select Trip" 10 10 8 White
                , TextDisplay "Equipment" 15 55 10 White
                , TextDisplay costTxt 20 100 4 Green
                ]
        rOpts = (\(k, t) -> SelectOption t k True False) <$> rEs'
        wasChsn t = elem t chsn
        aOpts = (\(k, t) -> SelectOption t k (wasChsn k) True) <$> aEs'
        opts' = rOpts ++ aOpts
        update keys p = TripEquipmentSelect gd loc keys p
        act keys = TripReview gd loc keys
        back = TripDestinationSelect gd


reviewTripMenu :: GameData -> T.Text -> [T.Text] -> GameConfigs -> Menu
reviewTripMenu gd loc eqs cfgs = mkMenu words [] (selOneOpts 80 185 3 4 opts (CursorRect White)) 0
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
        opts = [ MenuAction "Start Trip" (if enoughFunds then TripProgress gd'' atmpts else TripReview gd loc eqs)
               , MenuAction "Back to equipment" (TripEquipmentSelect gd loc eqs 0)
               , MenuAction "Abort Trip" (ResearchCenter gd)
               ]

tripProgressMenu :: GameData -> TripState -> GameConfigs -> InputState -> TimeoutView
tripProgressMenu gd tp cfgs (InputState _ ts) =
    case tripTries tp of
        [] -> TimeoutView ts 0 v $ TripResults gd tp
        (h:tl) ->
            let (gd', sfM) = exec h
                tp' = newTrip sfM tl
            in TimeoutView ts 2 v $ SharkFound gd' sfM tp'
    where
        curA = length $ tripTries tp
        allA = tripTotalTries tp
        v = View words [] [backRect, progressRect]
        progX = 80
        progY = 150
        progH = 20
        backRect = (Gray, progX, progY, 100, progH)
        p = floor ((fromIntegral (allA - curA)) / (fromIntegral allA) * 100)
        progressRect = (Green, progX, progY, p, progH)
        words = [ TextDisplay "Trip Progress" 10 10 8 White
                , TextDisplay "Looking for sharks..." 20 100 5 Blue
                ]
        newTrip sfM tl = case sfM of
                            Nothing -> tp { tripTries = tl }
                            Just sf -> tp { tripTries = tl, sharkFinds = (sharkFinds tp) ++ [sf]}
        exec it = executeTrip (sharkCfgs cfgs) gd (trip tp) it

sharkFoundMenu :: GameData -> Maybe SharkFind -> TripState -> GameConfigs -> Menu
sharkFoundMenu gd sfM tp cfgs = mkMenu words [] (selOneOpts 80 185 3 4 opts (CursorRect White)) 0
    where
        typeText sf = T.append (T.append "You " (findDataType sf)) " a "
        sharkText sf = sharkName ((sharks (sharkCfgs cfgs)) M.! (findSpecies sf))
        words = case sfM of
                    Nothing -> [ TextDisplay "No Shark" 40 10 10 White
                               , TextDisplay "Found" 70 50 10 White
                               , TextDisplay "Better luck next time!" 20 100 5 White
                               ]
                    Just sf -> [ TextDisplay (typeText sf) 10 10 8 White
                               , TextDisplay (sharkText sf) 20 80 5 Blue
                               ]
        nextState = if null (tripTries tp) then TripResults gd tp else TripProgress gd tp
        opts = [ MenuAction "Continue Trip" nextState ]

tripResultsMenu :: GameData -> TripState -> GameConfigs -> Menu
tripResultsMenu gd tp cfgs = mkMenu words [] (selOneOpts 60 185 3 4 opts (CursorRect White)) 0
    where
        sfMap = gameDataFoundSharks gd
        sfMap' = foldl (\m sf -> M.insertWith (\l sfL -> l ++ sfL) (findSpecies sf) [sf] m) sfMap (sharkFinds tp)
        gd' = gd { gameDataFoundSharks = sfMap' }
        words = [ TextDisplay "Trip Complete!" 10 10 8 White ]
        opts = [ MenuAction "Back to Research Center" (ResearchCenter gd') ]
