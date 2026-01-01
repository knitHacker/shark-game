{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.LabMenus
    ( labTopMenu
    , fleetManagementTopMenu
    , equipmentManagementTopMenu
    , equipmentStoreMenu
    , fundraiserTopMenu
    , boatStoreMenu
    , chooseActiveBoatMenu
    , donorList
    , fundraisingMenu
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
import Graphics.Animation
import Configs
import InputState

import Shark.Types
import Shark.Store

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Map.Strict ((!))
import Graphics.Animation (startAnimation, updateAnimation)

import Debug.Trace

labTopMenu :: GameData -> Graphics -> GameMenu
labTopMenu gd gr = GameMenu (textView (words ++ fundWords)) (Menu (selOneOpts 200 450 3 15 opts (Just backOpt) mc 0) Nothing)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
        words = [ TextDisplay "Lab" 40 20 10 White Nothing
                , TextDisplay "Management" 150 150 10 White Nothing
                ]
        fundWords = oneLine gr fundTxts 150 350 2
        opts = [ MenuAction "Fundraising" $ Just $ FundraiserTop gd
               , MenuAction "Fleet Management" $ Just $ FleetManagement gd
               , MenuAction "Equipment Management" $ Just $ EquipmentManagement gd
               ]
        backOpt = MenuAction "Return to Research Center" $ Just $ ResearchCenter gd

-- To make sure the user doesn't get stuck with no money they can "host" a fundraiser
-- This will give a small amount of money but also will add a new name for a donor to
-- the research center's name
fundraiserTopMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
fundraiserTopMenu gd cfgs gr = GameMenu (textView (words ++ fundWords)) (Menu (selOneOpts 250 450 4 15 opts (Just backOpt) mc 0) Nothing)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
        dateTxt = "Center research run time:"
        words = [ TextDisplay "Research" 40 20 10 White Nothing
                , TextDisplay "Center" 150 150 10 White Nothing
                --, TextDisplay dateTxt 200 300 3 White Nothing
                ]
        fundWords = oneLine gr fundTxts 200 350 2
        opts = [ MenuAction "Host Fundraiser" Nothing
               , MenuAction "View Donors" $ Just $ ViewDonors gd
               ]
        backOpt = MenuAction "Return to Lab Management" $ Just $ LabManagement gd

donorList :: GameData -> Graphics -> GameMenu
donorList gd gr = GameMenu (View words [] [] [] scrollData) (Menu (selOneOpts 200 550 3 10 [] (Just backOpt) mc 0) Nothing)
    where
        donors = []
        mc = CursorRect White
        donorDis = (\(t, i) -> TextDisplay t 150 (200 + i * 40) 3 LightGray Nothing) <$> zip donors [0..]
        words = [ (TextDisplay "Donor" 20 20 9 White Nothing, 0)
                , (TextDisplay "List" 80 150 9 White Nothing, 0)
                , (TextDisplay "No donors yet." 150 300 4 LightGray Nothing, 0)
                ]
        scrollData = mkScrollView gr donorDis [] 0 500 5
        backOpt = MenuAction "Return to Fundraising" $ Just $ FundraiserTop gd

fundraisingMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
fundraisingMenu gd cfgs gr = GameMenu (textView words) (Menu (selOneOpts 250 450 4 15 [] (Just backOpt) mc 0) Nothing)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
        dateTxt = "Center research run time:"
        words = [ TextDisplay "Research" 40 20 10 White Nothing
                , TextDisplay "Center" 150 150 10 White Nothing
                --, TextDisplay dateTxt 200 300 3 White Nothing
                ]
        backOpt = MenuAction "Return to Fundraising" $ Just $ FundraiserTop gd

boatBounceAnim :: Int -> Int -> Image -> AnimPlacement -> Int -> (ImagePlacement, AnimPlacement)
boatBounceAnim baseX baseY boatI ap frame =
    ( IPlace (baseX + xAdj) (baseY + yAdj) 1.3 boatI 2
    , ap { animPosX = baseX + 85 + xAdj, animPosY = baseY + 55 + yAdj }
    )
    where
        xAdj = case mod frame 8 of
            0 -> -5
            1 -> -5
            2 -> 0
            3 -> 0
            4 -> 5
            5 -> 5
            6 -> 0
            7 -> 0
            _ -> 0
        yAdj = case mod (frame + 1) 6 of
            0 -> 0
            1 -> 5
            2 -> 10
            3 -> 15
            4 -> 10
            5 -> 5
            _ -> 0

fleetManagementTopMenu :: GameData -> GameConfigs -> InputState -> Graphics -> GameView
fleetManagementTopMenu gd cfgs inputs gr = GameView v Nothing [to, toFlag] $ Just md
    where
        v = View ((,0) <$> words) [waterImg, boatStart, dockImg] [apStart] [] Nothing
        md = Menu (selOneOpts 200 550 4 15 opts (Just backOpt) mc 0) Nothing
        to = TimeoutData (timestamp inputs) 300 $ TimeoutAnimation $ startAnimation 10 nextFrame
        toFlag = TimeoutData (timestamp inputs) 150 $ TimeoutAnimation $ startTextAnim gr
        myBoat = gameActiveBoat $ gameDataEquipment gd
        boatInfo = boats (sharkCfgs cfgs) ! myBoat
        boatI = boatImage boatInfo
        mc = CursorRect White
        slotTxt = "Equipment Slots: " <> (T.pack . show $ boatEquipmentSlots boatInfo)
        fuelTxt = "Fuel Cost: $" <> (T.pack . show $ boatFuelCost boatInfo)
        words = [ TextDisplay "Fleet" 20 20 8 White Nothing
                , TextDisplay "Management" 50 125 8 White Nothing
                , TextDisplay "Boat Information" 75 275 3 LightGray Nothing
                , TextDisplay (boatName boatInfo) 125 350 3 LightGray Nothing
                , TextDisplay slotTxt 75 400 3 LightGray Nothing
                , TextDisplay fuelTxt 75 475 3 LightGray Nothing
                ]
        imgBaseX = 750
        imgBaseY = 250
        baseBoatX = imgBaseX + 115
        baseBoatY = imgBaseY + 50
        waterImg = IPlace imgBaseX imgBaseY 0.9 "water" 1
        dockImg = IPlace imgBaseX imgBaseY 0.9 "dock" 3
        boatStart = IPlace baseBoatX baseBoatY 1.3 boatI 2
        apStart = APlace (baseBoatX + 85) (baseBoatY + 55) 0.9 "flag" 0 0 4 True
        opts = [ MenuAction "Change Boats" $ Just $ ChooseBoat gd
               , MenuAction "Boat Store" $ Just $ BoatStore Nothing gd
                ]
        backOpt = MenuAction "Return to Management" $ Just $ LabManagement gd
        nextFrame (View ws _ aps _ _) frame =
                let (img, ap) = boatBounceAnim baseBoatX baseBoatY boatI (head aps) frame
                in View ws [waterImg, img, dockImg] [ap] [] Nothing

chooseActiveBoatMenu :: GameData -> GameConfigs -> GameMenu
chooseActiveBoatMenu gd cfgs = GameMenu (textView words) (Menu (md pos) Nothing)
    where
        md = scrollOpts 250 550 4 8 (OALOpts opts Nothing Nothing mc) (Just cancelOpt) [] 4
        cancelOpt = MenuAction "Back" $ Just $ FleetManagement gd
        mc = CursorRect White
        bts = boats $ sharkCfgs cfgs
        owned = gameOwnedBoats $ gameDataEquipment gd
        available = L.sortOn (\(_, cfg) -> boatEquipmentSlots cfg) $ M.assocs $ M.filterWithKey (\k _ -> k `elem` owned) bts
        pos = fromMaybe 0 $ L.findIndex (\(b, _) -> b == gameActiveBoat (gameDataEquipment gd)) available
        boatsOpts = (\(b, bCfg) -> (b, T.concat [boatName bCfg, " - ", T.pack (show (boatEquipmentSlots bCfg)), " slots"])) <$> available
        words = [ TextDisplay "Choose" 20 20 8 White Nothing
                , TextDisplay "Active Boat" 80 150 8 White Nothing
                , TextDisplay "Current Boat:" 200 275 5 LightGray Nothing
                , TextDisplay (boatName $ bts ! gameActiveBoat (gameDataEquipment gd)) 350 360 4 Green Nothing
                , TextDisplay "Owned Boats" 400 450 4 LightGray Nothing
                ]
        gd' ab = gd { gameDataEquipment = (gameDataEquipment gd) { gameActiveBoat = ab } }
        opts = (\(b, txt) -> MenuAction txt $ Just $ ChooseBoat (gd' b)) <$> boatsOpts

boatStoreMenu :: Maybe (T.Text, Int, GameData) -> GameData -> GameConfigs -> Graphics -> GameMenu
boatStoreMenu popupGd gd cfgs gr = GameMenu v $ Menu (md 0) pop
    where
        v = View ((,0) <$> words) [] [] [] Nothing
        md = scrollOptsBasic 100 300 3 10 colOpts Nothing opts 6
        mc = CursorRect White
        bts = boats $ sharkCfgs cfgs
        owned = gameOwnedBoats $ gameDataEquipment gd
        available = M.withoutKeys bts $ S.fromList owned
        hds = ["Boat", "Size (slots)", "Price"]
        colOpts = BasicCBOpts $ CBOpts "Buy" 350 hds $ mkBoatStoreEntry gd <$> M.assocs available
        words = [ TextDisplay "Boat" 20 20 9 White Nothing
                , TextDisplay "Store" 80 150 9 White Nothing
                ]
        opts = [ MenuAction "Leave Boat Store" $ Just $ FleetManagement gd
               ]
        pop = buyConfirm gd cfgs (BoatStore Nothing) <$> popupGd


equipmentManagementTopMenu :: GameData -> GameConfigs -> Graphics -> GameMenu
equipmentManagementTopMenu gd cfgs gr = GameMenu (View ((,0) <$> words) [] [] [] scrollData) (Menu (selOneOpts 150 650 3 15 opts (Just backOpt) mc 0) Nothing)
    where
        myEquipment = gameOwnedEquipment $ gameDataEquipment gd
        equipmentInfo = (!) (equipment (sharkCfgs cfgs)) <$> myEquipment
        mc = CursorRect White
        equipTxts = (\e -> equipText e <> "    Equipment Slots: " <> (T.pack . show $ equipSize e)) <$> equipmentInfo
        equipDis = (\(t, i) -> TextDisplay t 150 (375 + i * 50) 3 LightGray Nothing) <$> zip equipTxts [0..]
        words = [ TextDisplay "Equipment" 30 10 8 White Nothing
                , TextDisplay "Management" 175 120 8 White Nothing
                , TextDisplay "Owned Equipment" 300 275 4 LightGray Nothing
                ]
        scrollData = mkScrollView gr equipDis [] 0 570 5
        opts = [ MenuAction "Equipment Store" $ Just $ EquipmentStore Nothing gd ]
        backOpt = MenuAction "Return to Management" $ Just $ LabManagement gd

buyConfirm :: GameData -> GameConfigs -> (GameData -> GamePlayState) -> (T.Text, Int, GameData) -> MenuPopup GamePlayState
buyConfirm gd cfgs gps (item, price, gd') = MenuPopup v md 200 100 900 600 DarkBlue
    where
        v = View ((,0) <$> words) [] [] [] Nothing
        md = selOneOpts 350 500 3 4 opts (Just backOpt) (CursorRect White) 0
        funds = gameDataFunds gd
        enoughFunds = funds >= price
        fundTxt = T.append "Current Funds: " (showMoney funds)
        itemTxt = T.append "Item Cost: " (showMoney price)
        afterTxt = T.append "After Purchase: " (showMoney (funds - price))
        words = [ TextDisplay item 250 150 9 White Nothing
                , TextDisplay fundTxt 300 300 3 Green Nothing
                , TextDisplay itemTxt 300 350 3 Red Nothing
                , TextDisplay afterTxt 300 400 3 (if enoughFunds then Green else Red) Nothing
                ]
        opts = [ MenuAction "Confirm Purchase" $ if enoughFunds then Just (gps gd') else Nothing ]
        backOpt = MenuAction "Cancel" $ Just $ gps gd


mkBoatStoreEntry :: GameData -> (T.Text, Boat) -> ColumnAction GamePlayState
mkBoatStoreEntry gd (k, e) = ColAction txt action
    where
        cost = boatPrice e
        name = boatName e
        price = T.pack $ "$" ++ show cost
        txt = [name, T.concat [T.pack (show (boatEquipmentSlots e)), " slot(s)"], price] -- [Text]
        confirmBuy = buyBoat gd k cost
        action = if boatPrice e > gameDataFunds gd then Nothing else Just (BoatStore (Just (name, cost, confirmBuy)) gd)



mkStoreEntry :: GameData -> (T.Text, GameEquipment) -> ColumnAction GamePlayState
mkStoreEntry gd (k, e) = ColAction txt action
    where
        cost = equipPrice e
        name = equipText e
        price = T.pack $ "$" ++ show cost
        infoType = case equipInfoType e of
            Caught -> "caught"
            Observed -> "observed"
        txt = [name, infoType, price] -- [Text]
        confirmBuy = buyEquipment gd k cost
        action = if equipPrice e > gameDataFunds gd then Nothing else Just (EquipmentStore (Just (name, cost, confirmBuy)) gd)


equipmentStoreMenu :: Maybe (T.Text, Int, GameData) -> GameData -> GameConfigs -> Graphics -> GameMenu
equipmentStoreMenu popupGd gd cfgs gr = GameMenu v $ Menu md pop
    where
        v = View ((,0) <$> words) [] [] [] Nothing
        md = scrollOptsBasic 100 300 3 10 colOpts (Just backOpt) [] 6 0
        mc = CursorRect White
        equips = equipment $ sharkCfgs cfgs
        owned = gameOwnedEquipment $ gameDataEquipment gd
        available = M.withoutKeys equips (S.fromList owned)
        hds = ["Equipment", "Type", "Price"]
        colOpts = BasicCBOpts $ CBOpts "Buy" 350 hds $ mkStoreEntry gd <$> M.assocs available
        words = [ TextDisplay "Equipment" 20 20 9 White Nothing
                , TextDisplay "Store" 80 150 9 White Nothing
                ]
        backOpt = MenuAction "Leave Equipment Store" $ Just $ EquipmentManagement gd
        pop = buyConfirm gd cfgs (EquipmentStore Nothing) <$> popupGd
