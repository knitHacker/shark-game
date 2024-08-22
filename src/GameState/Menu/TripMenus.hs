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

import Debug.Trace

mapMenu :: GameData -> GameConfigs -> Menu
mapMenu gd cfgs = mkMenu words [] (selOneOpts 15 140 4 8 opts' mc) 0
    where
        locs = (\(loc, lCfg) -> (lCfg, showText lCfg)) <$> (M.assocs $ siteLocations $ sharkCfgs cfgs)
        mc = CursorRect Gray
        words = [ TextDisplay "Select Trip" 10 10 8 White
                , TextDisplay "Destination" 15 55 10 White
                ]
        opts = (\(loc, txt) -> MenuAction txt (TripEquipmentSelect gd loc [] 0)) <$> locs
        rtOpt = MenuAction "Return to Lab" $ ResearchCenter gd
        opts' = opts ++ [rtOpt]

equipmentPickMenu :: GameData -> GameLocation -> [T.Text] -> Int -> GameConfigs -> Menu
equipmentPickMenu gd loc chsn pos cfgs = mkMenu words [] (selMultOpts 15 130 3 6 opts' update act (Just back)) pos
    where
        eq = equipment $ sharkCfgs cfgs
        rEs = requiredEquipment loc
        aEs = allowedEquipment loc
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


reviewTripMenu :: GameData -> GameLocation -> [T.Text] -> GameConfigs -> Menu
reviewTripMenu gd loc eqs cfgs = mkMenu words [] (selOneOpts 80 185 3 4 opts (CursorRect Gray)) 0
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
tripProgressMenu gd tp cfgs (InputState _ ts) = TimeoutView ts 4 v (SharkFound gd Nothing tp')
    where
        curA = attempts tp
        allA = tripTotalTries tp
        v = View words [] [backRect, progressRect]
        progX = 80
        progY = 150
        progH = 20
        backRect = (Gray, progX, progY, 100, progH)
        p = floor ((fromIntegral curA) / (fromIntegral allA) * 100)
        progressRect = (Green, progX, progY, p, progH)
        words = [ TextDisplay "Trip Progress" 10 10 8 White
                ]
        tp' = tp { attempts = curA + 1 }

sharkFoundMenu :: GameData -> Maybe SharkFind -> TripState -> GameConfigs -> Menu
sharkFoundMenu gd sfM tp cfgs = mkMenu words [] (selOneOpts 80 185 3 4 opts (CursorRect Gray)) 0
    where
        sharkText sf = T.append (T.append (findDataType sf) " a ") (findSpecies sf)
        words = case sfM of
                    Nothing -> [ TextDisplay "No Shark" 40 10 10 White
                               , TextDisplay "Found" 70 50 10 White
                               , TextDisplay "Better luck next time!" 20 100 5 White
                               ]
                    Just sf -> [ TextDisplay (sharkText sf) 10 10 10 White ]
        nextState = if attempts tp == tripTotalTries tp then TripResults gd tp else TripProgress gd tp
        opts = [ MenuAction "Continue Trip" nextState ]

tripResultsMenu :: GameData -> TripState -> GameConfigs -> Menu
tripResultsMenu gd tp cfgs = mkMenu words [] (selOneOpts 60 185 3 4 opts (CursorRect Gray)) 0
    where
        words = [ TextDisplay "Trip Complete!" 10 10 8 White ]
        opts = [ MenuAction "Back to Research Center" (ResearchCenter gd) ]
