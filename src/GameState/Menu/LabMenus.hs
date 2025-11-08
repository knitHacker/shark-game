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
import InputState

import Shark.Types
import Shark.Store

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Map.Strict ((!))
import Graphics.Animation (startAnimation, updateAnimation)

import Debug.Trace

labTopMenu :: GameData -> Graphics -> GameMenu
labTopMenu gd gr = GameMenu (View (words ++ fundWords) [] [] Nothing) (Menu (selOneOpts 15 140 4 12 opts mc 0) Nothing)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
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
fundraiserTopMenu :: GameData -> Graphics -> GameMenu
fundraiserTopMenu gd gr = GameMenu (View (words ++ fundWords) [] [] Nothing) (Menu (selOneOpts 15 160 4 15 opts mc 0) Nothing)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
        dateTxt = "Center research run time:"
        words = [ TextDisplay "Research" 10 10 10 White
                , TextDisplay "Center" 40 45 10 White
                , TextDisplay dateTxt 35 105 3 White
                ]
        fundWords = oneLine gr fundTxts 35 90 2
        opts = [ MenuAction "Host Fundraiser" Nothing
               , MenuAction "Equipment Management" Nothing
               ]

boatBounceAnim :: Image -> Int -> [(Int, Int, Double, Image)]
boatBounceAnim boatI frame =
    [ (120, 85, 0.25, "water")
    , (147 + yAdj, 105 + xAdj , 0.4, boatI)
    , (115, 85, 0.25, "dock")
    ]
    where
        xAdj = case mod frame 8 of
            0 -> -1
            1 -> -1
            2 -> 0
            3 -> 0
            4 -> 1
            5 -> 1
            6 -> 0
            7 -> 0
            _ -> 0
        yAdj = case mod (frame + 1) 6 of
            0 -> 0
            1 -> 1
            2 -> 2
            3 -> 3
            4 -> 2
            5 -> 1
            _ -> 0


fleetManagementTopMenu :: GameData -> GameConfigs -> InputState -> Graphics -> GameView
fleetManagementTopMenu gd cfgs inputs gr = GameView v Nothing to $ Just md
    where
        v = View words imgs [] Nothing
        md = Menu (selOneOpts 15 190 4 15 opts mc 0) Nothing
        to = Just $ TimeoutData (timestamp inputs) 300 $ TimeoutAnimation $ startAnimation 10 nextFrame
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
        imgs = boatBounceAnim boatI 0
        opts = [ MenuAction "Boat Store" Nothing
               , MenuAction "Return to Management" $ Just $ LabManagement gd
               ]
        nextFrame frame = View words (boatBounceAnim boatI frame) [] Nothing

equipmentManagementTopMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
equipmentManagementTopMenu gd cfgs gr = GameMenu (View words [] [] scrollData) (Menu (selOneOpts 15 190 4 15 opts mc 0) Nothing)
    where
        myEquipment = gameOwnedEquipment $ gameDataEquipment gd
        equipmentInfo = (!) (equipment (sharkCfgs cfgs)) <$> myEquipment
        mc = CursorRect White
        equipTxts = (\e -> equipText e <> "    Equipment Slots: " <> (T.pack . show $ equipSize e)) <$> equipmentInfo
        equipDis = (\(t, i) -> TextDisplay t 20 (100 + i * 20) 3 LightGray) <$> zip equipTxts [0..]
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
buyConfirm gd cfgs (item, price, gd') = MenuPopup v md 20 40 150 200 DarkBlue
    where
        v = View words [] [] Nothing
        md = selOneOpts 80 190 3 4 opts (CursorRect White) 0
        funds = gameDataFunds gd
        enoughFunds = funds >= price
        fundTxt = T.append "Current Funds: " (showMoney funds)
        itemTxt = T.append "Item Cost: " (showMoney price)
        afterTxt = T.append "After Purchase: " (showMoney (funds - price))
        words = [ TextDisplay item 25 55 9 White
                , TextDisplay fundTxt 30 105 3 Green
                , TextDisplay itemTxt 30 125 3 Red
                , TextDisplay afterTxt 30 145 3 (if enoughFunds then Green else Red)
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


equipmentStoreMenu :: GameData -> Maybe (T.Text, Int, GameData) -> GameConfigs -> Graphics -> GameMenu
equipmentStoreMenu gd popupGd cfgs gr = GameMenu v $ Menu md pop
    where
        v = View words [] [] Nothing
        md = scrollOpts 15 80 3 8 colOpts opts 6 0
        mc = CursorRect White
        equips = equipment $ sharkCfgs cfgs
        owned = gameOwnedEquipment $ gameDataEquipment gd
        available = M.withoutKeys equips (S.fromList owned)
        hds = ["Equipment", "Type", "Price"]
        colOpts = BasicCBOpts $ CBOpts "Buy" 70 hds $ mkStoreEntry gd <$> M.assocs available
        words = [ TextDisplay "Equipment" 10 10 10 White
                , TextDisplay "Store" 40 45 10 White
                ]
        opts = [ MenuAction "Leave Equipment Store" $ Just $ EquipmentManagement gd
               ]
        pop = buyConfirm gd cfgs <$> popupGd
