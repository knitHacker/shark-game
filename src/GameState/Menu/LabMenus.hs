{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.LabMenus
    ( labTopMenu
    , fleetManagementTopMenu
    ) where


import GameState.Types
import OutputHandles.Types
import OutputHandles.Util

import SaveData
import GameState.Types
import Shark.Trip
import Graphics.Types
import Graphics.TextUtil
import Graphics.Menu
import Configs

import Shark.Types

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

labTopMenu :: GameData -> Graphics -> Menu GamePlayState
labTopMenu gd gr = mkMenu (words ++ fundWords) [] Nothing (selOneOpts 15 140 4 12 opts mc 0)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (T.append "$" (T.pack (show funds)), Green, 3)]
        words = [ TextDisplay "Lab" 10 10 10 White
                , TextDisplay "Management" 40 45 10 White
                ]
        fundWords = oneLine gr fundTxts 35 90 2
        opts = [ MenuAction "Fundraising" False ComingSoon
               , MenuAction "Fleet Management" True $ FleetManagement gd
               , MenuAction "Equipment Management" False ComingSoon
               , MenuAction "Return to Research Center" True $ ResearchCenter gd
               ]

-- To make sure the user doesn't get stuck with no money they can "host" a fundraiser
-- This will give a small amount of money but also will add a new name for a donor to
-- the research center's name
fundraiserTopMenu :: GameData -> Graphics -> Menu GamePlayState
fundraiserTopMenu gd gr = mkMenu (words ++ fundWords) [] Nothing (selOneOpts 15 160 4 15 opts mc 0)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (T.append "$" (T.pack (show funds)), Green, 3)]
        dateTxt = "Center research run time:"
        words = [ TextDisplay "Research" 10 10 10 White
                , TextDisplay "Center" 40 45 10 White
                , TextDisplay dateTxt 35 105 3 White
                ]
        fundWords = oneLine gr fundTxts 35 90 2
        opts = [ MenuAction "Host Fundraiser" False ComingSoon
               , MenuAction "Equipment Management" False ComingSoon
               ]


fleetManagementTopMenu :: GameData -> GameConfigs -> Graphics -> Menu GamePlayState
fleetManagementTopMenu gd cfgs gr = mkMenu words imgs Nothing (selOneOpts 15 190 4 15 opts mc 0)
    where
        myBoat = gameBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! myBoat
        boatI = boatImage boatInfo
        mc = CursorRect White
        slotTxt = "Equipment Slots: " <> (T.pack . show $ boatEquipmentSlots boatInfo)
        fuelTxt = "Fuel Cost: $" <> (T.pack . show $ boatFuelCost boatInfo)
        words = [ TextDisplay "Fleet" 10 10 10 White
                , TextDisplay "Management" 40 45 10 White
                , TextDisplay "Boat Information" 10 80 4 LightGray
                , TextDisplay (boatName boatInfo) 10 110 3 LightGray
                , TextDisplay slotTxt 10 130 3 LightGray
                , TextDisplay fuelTxt 10 150 3 LightGray
                ]
        imgs = [ (120, 85, 0.25, "water")
               , (150, 105, 0.35, boatI)
               , (115, 85, 0.25, "dock")
               ]
        opts = [ MenuAction "Boat Store" False ComingSoon
               , MenuAction "Return to Management" True $ LabManagement gd
               ]
