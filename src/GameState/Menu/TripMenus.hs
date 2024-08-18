{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.TripMenus
    ( mapMenu
    , equipmentPickMenu
    , reviewTripMenu
    ) where

import SaveData
import Configs
import GameState.Types
import OutputHandles.Types

import qualified Data.Map.Strict as M
import qualified Data.Text as T


mapMenu :: GameData -> GameConfigs -> Menu
mapMenu gd cfgs = Menu words [] (selOneOpts 15 140 opts' mc) 0
    where
        locs = (\(loc, lCfg) -> (lCfg, showText lCfg)) <$> (M.assocs $ siteLocations $ sharkCfgs cfgs)
        mc = CursorRect Gray
        words = [ TextDisplay "Select Trip Destination" 10 10 220 80 White
                ]
        opts = (\(loc, txt) -> MenuAction txt (TripEquipmentSelect gd loc [] 0)) <$> locs
        rtOpt = MenuAction "Return to Lab" $ ResearchCenter gd
        opts' = opts ++ [rtOpt]

equipmentPickMenu :: GameData -> GameLocation -> [T.Text] -> Int -> GameConfigs -> Menu
equipmentPickMenu gd loc chsn pos cfgs = Menu words [] (selMultOpts 15 120 opts' update act (Just back)) pos
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
        words = [ TextDisplay "Select Trip Equipment" 10 10 220 80 White
                , TextDisplay costTxt 10 90 100 15 White
                ]
        rOpts = (\(k, t) -> SelectOption t k True False) <$> rEs'
        wasChsn t = elem t chsn
        aOpts = (\(k, t) -> SelectOption t k (wasChsn k) True) <$> aEs'
        opts' = rOpts ++ aOpts
        update keys p = TripEquipmentSelect gd loc keys p
        act keys = TripReview gd loc keys
        back = TripDestinationSelect gd


reviewTripMenu :: GameData -> GameLocation -> [T.Text] -> GameConfigs -> Menu
reviewTripMenu gd loc eqs cfgs = Menu words [] (selOneOpts 80 180 opts (CursorRect Gray)) 0
    where
        trip = tripInfo (sharkCfgs cfgs) loc eqs
        funds = gameDataFunds gd
        tc = tripCost trip
        enoughFunds = funds >= tc
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        tripTxt = T.append "Trip Cost: $" (T.pack (show tc))
        afterTxt = T.append "After Trip: $" (T.pack (show (funds - tc)))
        tripLenTxt = T.append (T.append "Trip Length: " (T.pack (show (tripLength trip)))) " month(s)"
        words = [ TextDisplay "Review Trip Details" 10 10 220 80 White
                , TextDisplay fundTxt 25 95 85 12 Green
                , TextDisplay tripTxt 25 115 80 12 Red
                , TextDisplay afterTxt 25 135 80 12 (if enoughFunds then Green else Red)
                , TextDisplay tripLenTxt 120 95 80 12 White
                ]
        gd' = gd { gameDataFunds = funds - tc, gameDataMonth = gameDataMonth gd + tripLength trip }
        opts = [ MenuAction "Start Trip" (if enoughFunds then ResearchCenter gd' else TripReview gd loc eqs)
               , MenuAction "Back to equipment" (TripEquipmentSelect gd loc eqs 0)
               , MenuAction "Abort Trip" (ResearchCenter gd)
               ]
