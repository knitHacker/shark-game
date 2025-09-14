{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.LabMenus
    ( labTopMenu
    , fleetManagementTopMenu
    , equipmentManagementTopMenu
    , equipmentStoreMenu
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
import Shark.Store

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
        opts = [ MenuAction "Fundraising" Nothing
               , MenuAction "Fleet Management" $ Just $ FleetManagement gd
               , MenuAction "Equipment Management" $ Just $ EquipmentManagement gd
               , MenuAction "Return to Research Center" $ Just $ ResearchCenter gd
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
        opts = [ MenuAction "Host Fundraiser" Nothing
               , MenuAction "Equipment Management" Nothing
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
        opts = [ MenuAction "Boat Store" Nothing
               , MenuAction "Return to Management" $ Just $ LabManagement gd
               ]

equipmentManagementTopMenu :: GameData -> GameConfigs -> Graphics -> Menu GamePlayState
equipmentManagementTopMenu gd cfgs gr = mkMenu words [] Nothing (selOneOpts 15 190 4 15 opts mc 0)
    where
        myEquipment = gameOwnedEquipment $ gameDataEquipment gd
        equipmentInfo = (!) (equipment (sharkCfgs cfgs)) <$> myEquipment
        mc = CursorRect White
        equipTxts = (\e -> (equipText e) <> "\tEquipment Slots: " <> (T.pack . show $ equipSize e)) <$> equipmentInfo
        equipDis = (\(t, i) -> TextDisplay t 20 (160 + i * 20) 3 LightGray) <$> zip equipTxts [0..]
        words = [ TextDisplay "Equipment" 10 10 10 White
                , TextDisplay "Management" 40 45 10 White
                , TextDisplay "Owned Equipment" 10 80 4 LightGray
                ]
        equipTxt = map (\(e, i) -> TextDisplay (equipText (equipment (sharkCfgs cfgs) ! e)) 20 (160 + i * 20) 3 LightGray) (zip myEquipment [0..])
        scrollData = mkScrollView gr equipDis [] 0 190 5
        opts = [ MenuAction "Equipment Store" $ Just $ EquipmentStore gd Nothing
               , MenuAction "Return to Management" $ Just $ LabManagement gd
               ]

buyConfirm :: GameData -> GameConfigs -> (T.Text, Int, GameData) -> MenuPopup GamePlayState
buyConfirm gd cfgs (item, price, gd') = MenuPopup m 50 50 300 200 DarkBlue
    where
        m = mkMenu words [] Nothing (selOneOpts 80 220 3 2 opts (CursorRect White) 0)
        funds = gameDataFunds gd
        enoughFunds = funds >= price
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        itemTxt = T.append "Item Cost: $" (T.pack (show price))
        afterTxt = T.append "After Purchase: $" (T.pack (show (funds - price)))
        words = [ TextDisplay "Confirm Purchase" 10 10 14 White
                , TextDisplay item 15 55 10 White
                , TextDisplay fundTxt 25 105 3 Green
                , TextDisplay itemTxt 25 125 3 (if enoughFunds then Green else Red)
                , TextDisplay afterTxt 25 145 3 (if enoughFunds then Green else Red)
                ]
        opts = [ MenuAction "Confirm Purchase" $ if enoughFunds then Just (EquipmentStore gd' Nothing) else Nothing
               , MenuAction "Cancel" $ Just $ EquipmentStore gd Nothing
               ]

mkStoreEntry :: GameData -> (T.Text, GameEquipment) -> ColumnAction GamePlayState
mkStoreEntry gd (k, e) = ColAction txt action
    where
        cost = equipPrice e
        name = equipText e
        price = T.pack $ "$" ++ show cost
        txt = [name, equipInfoType e, price] -- [Text]
        confirmBuy = buyEquipment gd k cost
        action = if equipPrice e > gameDataFunds gd then Nothing else Just (EquipmentStore gd (Just (name, cost, confirmBuy)))


equipmentStoreMenu :: GameData -> Maybe (T.Text, Int, GameData) -> GameConfigs -> Graphics -> Menu GamePlayState
equipmentStoreMenu gd popupGd cfgs gr = mkMenuPop words [] Nothing pop (scrollOpts 15 80 3 8 colOpts opts 6 0)
    where
        mc = CursorRect White
        equips = equipment $ sharkCfgs cfgs
        owned = gameOwnedEquipment $ gameDataEquipment gd
        available = M.withoutKeys equips (S.fromList owned)
        hds = ["Equipment", "Type", "Price"]
        colOpts = BasicCBOpts $ CBOpts "Buy" 70 hds $ mkStoreEntry gd <$> M.assocs available
        words = [ TextDisplay "Equipment" 10 10 10 White
                , TextDisplay "Store" 40 45 10 White
                ]
        opts = [ MenuAction "Leave Equipment Store" $ Just $ LabManagement gd
               ]
        pop = buyConfirm gd cfgs <$> popupGd
