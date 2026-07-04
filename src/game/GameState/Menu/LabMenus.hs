{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.LabMenus
    ( LabTopState(..)
    , initLabTopState
    , FundraiserTopState(..)
    , initFundraiserTopState
    , DonorListState(..)
    , initDonorList
    , FundraisingState(..)
    , initFundraisingState
    , FleetManagementState(..)
    , initFleetManagementState
    , BoatStoreState(..)
    , initBoatStoreState
    , EquipManagementState(..)
    , initEquipManagementState
    , EquipStoreState(..)
    , initEquipStoreState
    ) where

import GameState.Types
import OutputHandles.Types
import OutputHandles.Util

import SaveData
import GameState.Types
import Shark.Trip
import Shark.Util
import Graphics.Types
import Graphics.TextUtil
import Graphics.Asset
import GameState.Util
import Configs
import InputState

import Shark.Types
import Shark.Store

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Map.Strict ((!))

import Debug.Trace

data LabTopOpt = LabTopFundraising | LabTopFleet | LabTopEquip | LabTopReturnRC
               deriving (Show, Eq, Enum, Ord, Bounded)

data LabTopState = LabTopState (MenuStateInfo LabTopOpt)

initLabTopState :: GameData -> LabTopState
initLabTopState gd = LabTopState $ initMenuStateInfo gd

instance GamePlayStateE LabTopState where
    think gps@(LabTopState msi) _ = menuInfoThink LabTopState next back msi
        where
            gd = menuGetData msi
            back = Step $ TopTransition ResearchCenterMenu gd
            next LabTopFundraising = stepTransition $ initFundraiserTopState gd
            next LabTopFleet = stepTransition $ initFleetManagementState gd
            next LabTopEquip = stepTransition $ initEquipManagementState gd
            next LabTopReturnRC = back

    transition gps@(LabTopState msi) _ gr = (menuInfoApply msi gview gr id, [])
        where
            gview = GView assets mempty $ Just menu
            assets = M.fromList $ zip [0..]
                                      [ staticText "Lab" White 40 20 10 0
                                      , staticText "Management" White 150 150 10 0
                                      ]
            menu = DefaultMenu $ MenuAsset 200 450 1 Nothing 15 (mkItem <$> menuItems)
            mkItem txt = MenuItem (MText txt) Blue 3 0 0 True Nothing Nothing
            menuItems = ["Fundraising", "Fleet Management", "Equipment Management", "Return to Research Center"]

    update gps@(LabTopState msi) gv _ gr = menuInfoUpdate LabTopState msi gv gr id

data FundraiserTopOpt = HostFundraiserOpt | ViewDonorsOpt | BackLabManagementOpt
                      deriving (Show, Eq, Ord, Enum, Bounded)

data FundraiserTopState = FundraiserTopState (MenuStateInfo FundraiserTopOpt)

initFundraiserTopState :: GameData -> FundraiserTopState
initFundraiserTopState gd = FundraiserTopState $ initMenuStateInfo gd

instance GamePlayStateE FundraiserTopState where
    think gps@(FundraiserTopState msi) _ = menuInfoThink FundraiserTopState next back msi
        where
            gd = menuGetData msi
            back = stepTransition $ initLabTopState gd
            next HostFundraiserOpt = stepTransition $ initFundraisingState gd
            next ViewDonorsOpt = stepTransition $ initDonorList gd
            next BackLabManagementOpt = back

    transition gps@(FundraiserTopState msi) _ gr = (menuInfoApply msi gview gr id, [])
        where
            gview = GView assets mempty $ Just menu
            assets = M.fromList $ zip [0..]
                                      [ staticText "Center" White 40 20 8 0
                                      , staticText "Fundraising" White 150 150 10 0
                                      ]
            menu = DefaultMenu $ MenuAsset 250 450 0 Nothing 3 (mkItem <$> mItems)
            mkItem txt = MenuItem (MText txt) Blue 4 0 0 True Nothing Nothing
            mItems = ["Host Fundraiser", "View Donors", "Return to Lab Management"]

    update gps@(FundraiserTopState msi) gv _ gr = menuInfoUpdate FundraiserTopState msi gv gr id

data DonorListState = DonorListState SimpleStateInfo

initDonorList :: GameData -> DonorListState
initDonorList gd = DonorListState $ SimpleInfo gd Nothing

instance GamePlayStateE DonorListState where
    think gps@(DonorListState si) _ = simpleInfoThink DonorListState (stepTransition (initFundraiserTopState gd)) si
        where
            gd = gamedata si

    transition gps@(DonorListState si) _ gr = (withPauseApply gr (pauseSelM si) gview, [])
        where
            gview = GView assets mempty $ Just menu
            assets = M.fromList $ zip [0..]
                                      [ staticText "Donor" White 20 20 9 0
                                      , staticText "List" White 80 150 9 0
                                      , staticText "No donors yet." LightGray 150 300 4 0
                                      ]
            menu = DefaultMenu $ MenuAsset 250 450 0 Nothing 3
                                           [ MenuItem (MText "Return to Fundraising") Blue 4 0 0 True (Just White) Nothing ]


data FundraisingState = FundraisingState SimpleStateInfo

initFundraisingState :: GameData -> FundraisingState
initFundraisingState gd = FundraisingState $ SimpleInfo gd Nothing

instance GamePlayStateE FundraisingState where
    think gps@(FundraisingState si) _ = simpleInfoThink FundraisingState (stepTransition (initLabTopState (gamedata si))) si

    transition gps@(FundraisingState si) _ gr = (withPauseApply gr (pauseSelM si) gview, [])
        where
            gd = gamedata si
            gview = GView assets mempty $ Just menu
            assets = M.fromList $ zip [0..]
                                      [ staticText "Raise" White 40 20 10 0
                                      , staticText "Funds" White 150 150 10 0
                                      , staticText currTxt Green 200 300 4 0
                                      ]
            currTxt = T.append "Current Funds: " (showMoney (gameDataFunds gd))
            menu = DefaultMenu $ MenuAsset 250 450 0 Nothing 3
                                           [ MenuItem (MText "Return to Lab Management") Blue 4 0 0 True (Just White) Nothing ]

data FleetMOpt = ChangeBoatsOpt | BoatStoreOpt | ReturnLabTop
               deriving (Show, Eq, Ord, Enum, Bounded)

data FleetManagementState = FleetManagementState GameData FleetMOpt (Maybe PauseOpt) [AnimationState]

initFleetManagementState :: GameData -> FleetManagementState
initFleetManagementState gd = FleetManagementState gd minBound Nothing []

--boatBounceAnim :: Int -> Int -> Image -> AnimPlacement -> Int -> (ImagePlacement, AnimPlacement)
--boatBounceAnim baseX baseY boatI ap frame =
--    ( IPlace (baseX + xAdj) (baseY + yAdj) 1.3 boatI 2
--    , ap { animPosX = baseX + 85 + xAdj, animPosY = baseY + 55 + yAdj }
--    )
--    where
--        xAdj = case mod frame 8 of
--            0 -> -5
--            1 -> -5
--            2 -> 0
--            3 -> 0
--            4 -> 5
--            5 -> 5
--            6 -> 0
--            7 -> 0
--            _ -> 0
--        yAdj = case mod (frame + 1) 6 of
--            0 -> 0
--            1 -> 5
--            2 -> 10
--            3 -> 15
--            4 -> 10
--            5 -> 5
--            _ -> 0

instance GamePlayStateE FleetManagementState where
    think gps@(FleetManagementState gd mOpt pSelM animS) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | enterJustPressed inputs =
            case (pSelM, mOpt) of
                (Just po, _) -> getPauseEnterAction po gd $ AnyGamePlayState $ FleetManagementState gd mOpt Nothing animS
                (_, ChangeBoatsOpt) -> stepTransition $ initChooseBoatState gd
                (_, BoatStoreOpt) -> stepTransition $ initBoatStoreState gd
                (_, ReturnLabTop) -> stepTransition $ initLabTopState gd
        | escapeJustPressed inputs && isNothing pSelM = stepInputUpdate $ FleetManagementState gd mOpt (Just minBound) animS
        | escapeJustPressed inputs = stepInputUpdate $ FleetManagementState gd mOpt Nothing animS
        | moveInputJustPressed inputs =
            case (inputDirection inputs, pSelM) of
                (Just dir, Just pSel) -> getPauseMoveAction dir pSel (\po -> AnyGamePlayState (FleetManagementState gd mOpt (Just po) animS))
                (Just dir, _) -> moveMenuPos dir mOpt (\newOpt -> FleetManagementState gd newOpt pSelM animS)
                _ -> Step NoChange
        | otherwise = Step $ getAnimationStep inputs animS

    transition gps@(FleetManagementState gd mSel pSelM _) cfgs gr = (gview, [])
        where
            pauseIdx = 0
            gview = GView assets (overlays pSelM) $ Just menu
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            overlays _ = mempty
            assets = M.fromList $
                zip [0..]
                    [ staticText "Fleet" White 20 20 8 0
                    , staticText "Management" White 50 125 8 0
                    , staticText "Boat Information" LightGray 75 275 3 0
                    , staticText (boatName boatInfo) LightGray 125 350 3 0
                    , staticText slotTxt LightGray 75 400 3 0
                    , staticText fuelTxt LightGray 75 475 3 0
                    , Asset (AssetImage "water" (scale gr)) (imgBaseX gr) imgBaseY 1 True  $ Just rsWater
                    , Asset (AssetImage boatI (scaleB gr)) (boatBaseX gr) (boatBaseY gr) 2 True  $ Just rsBoat
                    , Asset (AssetImage "dock" (scale gr)) (imgBaseX gr) imgBaseY 3 True  $ Just rsDock
                    , Asset (AssetAnimation "flag" 0 0 (scaleB gr)) (flagBaseX gr) (flagBaseY gr) 3 True $ Just rsFlag
                    ]
            myBoat = gameActiveBoat $ gameDataEquipment gd
            boatInfo = boats (sharkCfgs cfgs) ! myBoat
            boatI = boatImage boatInfo
            slotTxt = "Equipment Slots: " <> (T.pack . show $ boatEquipmentSlots boatInfo)
            fuelTxt = "Fuel Cost: $" <> (T.pack . show $ boatFuelCost boatInfo)
            rsWater ass gr' = ass { object = AssetImage "water" (scale gr'), assetX = imgBaseX gr' }
            rsBoat ass gr' = ass { object = AssetImage boatI (scaleB gr'), assetX = boatBaseX gr', assetY = boatBaseY gr' }
            rsFlag ass gr' = ass { object = AssetAnimation "flag" 0 0 (scaleB gr'), assetX = flagBaseX gr', assetY = flagBaseY gr' }
            rsDock ass gr' = ass { object = AssetImage "dock" (scale gr'), assetX = imgBaseX gr' }
            imgBaseX gr' = (graphicsWindowWidth gr' `div` 2)
            imgBaseY = 250
            boatOffX = 125
            boatOffY = 75
            flagOffX = boatOffX + 95
            flagOffY = boatOffY + 62
            boatBaseX gr' = imgBaseX gr' + round (scale gr' * boatOffX)
            boatBaseY gr' = imgBaseY + round (scale gr' * boatOffY)
            flagBaseX gr' = imgBaseX gr' + round (scale gr' * flagOffX)
            flagBaseY gr' = imgBaseY + round (scale gr' * flagOffY)
            scaleX gr' = ((fromIntegral (graphicsWindowWidth gr') / 2) - 100) / fromIntegral (imageSizeX (graphicsStaticTextures gr' M.! "water"))
            scaleY gr' = (fromIntegral (graphicsWindowHeight gr') - (490 + 250)) / fromIntegral (imageSizeY (graphicsStaticTextures gr' M.! "water"))
            scale gr' = scaleX gr' --min (scaleX gr') (scaleY gr')
            scaleB gr' = scale gr' * 1.5
            hlM cmp = if cmp == mSel then Just White else Nothing
            mY gr' = graphicsWindowHeight gr' - 250
            menu = DefaultMenu $ MenuAsset 150 (mY gr) 0 (Just mnRs) 10 items
            items = [ MenuItem (MText "Change Boats") Blue 3 0 0 True (hlM ChangeBoatsOpt) Nothing
                    , MenuItem (MText "Boat Store") Blue 3 0 0 True (hlM BoatStoreOpt) Nothing
                    , MenuItem (MText "Back") Blue 3 0 0 True (hlM ReturnLabTop) Nothing
                    ]
            mnRs ass gr' = ass { menuYBase = mY gr' }

    update gps@(FleetManagementState gd mSel pSelM _) gsn cfgs gr = withPauseUpdate gr gps pSelM nGv gsn
        where
            nGv gv = updateMenuHighlight (fromEnum mSel) White gv

    updateAnims (FleetManagementState gd mSel pSelM _) newAnims = FleetManagementState gd mSel pSelM newAnims


newtype ChooseBoatState = ChooseBoatState (MenuStateInfo Int)

initChooseBoatState :: GameData -> ChooseBoatState
initChooseBoatState gd = ChooseBoatState $ MenuInfo 0 $ SimpleInfo gd Nothing

instance GamePlayStateE ChooseBoatState where
    think gps@(ChooseBoatState msi) cfgs inputs = menuInfoIntThink ChooseBoatState mAct back maxMIdx msi inputs
        where
            back = stepTransition $ initFleetManagementState gd
            -- TODO: choose option
            mAct i
                | i >= 0 && i < maxMIdx = stepInputUpdate $ ChooseBoatState $ menuUpdateData msi (gd' (owned !! i))
                | i == maxMIdx = back
                | otherwise = Step NoChange
            maxMIdx = length owned
            owned = gameOwnedBoats $ gameDataEquipment gd
            gd = menuGetData msi
            gd' activeBoat = gd { gameDataEquipment = (gameDataEquipment gd) { gameActiveBoat = activeBoat } }

    transition gps@(ChooseBoatState msi) cfgs gr = (gview, [])
        where
            pauseIdx = 0
            pSelM = menuGetPause msi
            gview = GView assets (overlays pSelM) $ Just menu
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            overlays _ = mempty
            assets = M.fromList $ zip [0..]
                                      [ staticText boatN Green 460 340 4 0
                                      , staticText "Choose" White 20 20 6 0
                                      , staticText "Active Boat" White 80 120 8 0
                                      , staticText "Current Boat:" LightGray 200 240 4 0
                                      , staticText "Owned Boats" LightGray 400 420 4 0
                                      ]
            menu = DefaultMenu $ MenuAsset 150 500 0 Nothing 6 items
            items = btMenuItem ++ [MenuItem (MText "Back") Blue 3 0 0 True (hl (length boatsOpts)) Nothing]
            gd = menuGetData msi
            bts = boats $ sharkCfgs cfgs
            boatN = boatName $ bts ! gameActiveBoat (gameDataEquipment gd)
            owned = gameOwnedBoats $ gameDataEquipment gd
            boatsOpts = (\bk -> let bCfg = bts ! bk in T.concat [boatName bCfg, " - ", T.pack (show (boatEquipmentSlots bCfg)), " slots"]) <$> owned
            hl i = if menuSel msi == i then Just White else Nothing
            btMenuItem = (\(i, bt) -> MenuItem (MText bt) Blue 3 0 0 True (hl i) Nothing) <$> zip [0..] boatsOpts

    update gps@(ChooseBoatState msi) gsn cfgs gr = menuInfoUpdate ChooseBoatState msi gsn gr upBt
        where
            gd = menuGetData msi
            upBt gv = gv { assets = M.adjust upAss 0 (assets gv) }
            upAss ass = ass { object = AssetText boatN Green 4 }
            bts = boats $ sharkCfgs cfgs
            boatN = boatName $ bts ! gameActiveBoat (gameDataEquipment gd)

data BuyPopupOpt = BuyPopupConfirm | BuyPopupCancel
                 deriving (Show, Eq, Ord, Enum, Bounded)

boatStoreInfo :: GameData -> GameConfigs -> [(T.Text, Boat)]
boatStoreInfo gd cfgs = M.assocs available
    where
        bts = boats $ sharkCfgs cfgs
        owned = gameOwnedBoats $ gameDataEquipment gd
        available = M.withoutKeys bts $ S.fromList owned

slotText :: Boat -> T.Text
slotText bt = T.concat [T.pack (show slotNum), " slot", if slotNum == 1 then "" else "s"]
    where
        slotNum = boatEquipmentSlots bt

boatCostTxt :: Boat -> Int -> T.Text
boatCostTxt bt maxCost = T.justifyRight width ' ' $ showMoney $ boatPrice bt
    where
        width = T.length $ showMoney maxCost

data BoatPopupInfo = BoatPopupInfo
    { boatKey :: T.Text
    , boatDisplay :: T.Text
    , boatCost :: Int
    , popupSel :: BuyPopupOpt
    } -- TODO: probably will add img name to display image

data BoatStoreState = BoatStoreState (Either Int BoatPopupInfo) SimpleStateInfo

initBoatStoreState :: GameData -> BoatStoreState
initBoatStoreState gd = BoatStoreState (Left 0) $ SimpleInfo gd Nothing

initBoatPopupInfo :: (T.Text, Boat) -> BoatPopupInfo
initBoatPopupInfo (bk, boat) = BoatPopupInfo bk (boatName boat) (boatPrice boat) minBound

instance GamePlayStateE BoatStoreState where
    think gps@(BoatStoreState boatPopE si) cfgs inputs
        | moveInputJustPressed inputs && not (simpleInfoPaused si) =
            case (inputDirection inputs, boatPopE) of
                (Just DUp, Left 0) -> Step NoChange
                (Just DUp, Left idx) -> stepInputUpdate $ BoatStoreState (Left (idx - 1)) si
                (Just DDown, Left idx) | idx == availBoats -> Step NoChange
                (Just DDown, Left idx) -> stepInputUpdate $ BoatStoreState (Left (idx + 1)) si
                (Just DUp, Right bpi@(BoatPopupInfo _ _ _ BuyPopupCancel)) -> stepInputUpdate $ BoatStoreState (Right (bpi { popupSel = BuyPopupConfirm })) si
                (Just DDown, Right bpi@(BoatPopupInfo _ _ _ BuyPopupConfirm)) -> stepInputUpdate $ BoatStoreState (Right (bpi { popupSel = BuyPopupCancel })) si
                _ -> Step NoChange
        | otherwise = simpleInfoThink (BoatStoreState boatPopE) (enterAction boatPopE) si inputs
        where
            mAct idx = Step NoChange
            gd = gamedata si
            currFunds = gameDataFunds gd
            getPrice idx = boatPrice $ snd $ bList !! idx
            bList = boatStoreInfo gd cfgs
            availBoats = length bList
            newGD (bk, boat) msi' = menuUpdateData msi' (buyBoat gd bk (boatPrice boat))
            enterAction (Left idx)
                | idx < length bList && getPrice idx > currFunds = Step NoChange
                | idx < length bList = stepInputUpdate $ BoatStoreState (Right (initBoatPopupInfo (bList !! idx))) si
                | otherwise = stepTransition $ initFleetManagementState gd
            enterAction (Right (BoatPopupInfo bk _ _ BuyPopupCancel)) =
                case L.elemIndex bk (fst <$> bList) of
                    Nothing -> stepInputUpdate $ BoatStoreState (Left 0) si
                    Just idx -> stepInputUpdate $ BoatStoreState (Left idx) si
            enterAction (Right (BoatPopupInfo bk _ price BuyPopupConfirm)) =
                let gd' = buyBoat gd bk price
                in stepInputUpdate $ BoatStoreState (Left 0) $ si { gamedata = gd' }

    transition gps@(BoatStoreState popE si) cfgs gr = (gview, [])
        where
            pSelM = pauseSelM si
            gd = gamedata si
            bts = boatStoreInfo gd cfgs
            numBts = length bts
            maxCost = maximum $ (boatPrice . snd) <$> bts
            maxPriceStr = T.length $ showMoney maxCost
            headingFS = 3
            listingFS = 3
            yStart = div (graphicsWindowHeight gr) 2
            yRsM ma gr' = ma { menuYBase = div (graphicsWindowHeight gr') 2 }
            currFunds = gameDataFunds gd
            canAfford bt = boatPrice bt <= currFunds
            isBoatSel i = case popE of
                Left idx -> i == idx
                Right _  -> False
            isLeaveSel = case popE of
                Left idx -> idx == numBts
                Right _  -> False
            buyColor i bt
                | canAfford bt && isBoatSel i = White
                | canAfford bt                = Green
                | isBoatSel i                 = Red
                | otherwise                   = Gray
            headingRow = (Right [("Boat", Nothing, False), ("Size (slots)", Nothing, False), (T.justifyRight maxPriceStr ' ' "Cost", Nothing, False), (" ", Nothing, False)], headingFS, White, Nothing)
            boatRows = zipWith (\i (_, bt) ->
                (Right [(boatName bt, Nothing, False), (slotText bt, Nothing, False), (boatCostTxt bt maxCost, Nothing, False), ("Buy", Nothing, True)], listingFS, Blue, Just (buyColor i bt))
                ) [0..] bts
            leaveRow = (Left "Leave Boat Store", listingFS, Blue, if isLeaveSel then Just White else Nothing)
            menu = makeTableMenu gr (Just yRsM) (headingRow : boatRows ++ [leaveRow]) 60 60 100 yStart 10 1
            gview = GView assets (overlays' popE) $ Just menu
            assets = M.fromList $ zip [0..]
                                      [ staticText "Boat" White 20 20 9 0
                                      , staticText "Store" White 80 150 9 0
                                      ]
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            overlays _ = mempty
            overlays' (Left _) = overlays pSelM
            overlays' (Right bpi) = S.insert (buyBoatPopup bpi gd gr) $ overlays pSelM


buyBoatPopup :: BoatPopupInfo -> GameData -> Graphics -> Overlay
buyBoatPopup (BoatPopupInfo bk name price sel) gd gr = AOverlay 1 popAssets (Just menu) (oX gr) (oY gr) (oW gr) (oH gr) DarkGray $ Just rs
    where
        oW gr' = max 400 $ floor $ fromIntegral (graphicsWindowWidth gr') * 0.8
        oH gr' = max 300 $ floor $ fromIntegral (graphicsWindowHeight gr') * 0.7
        oX gr' = midStartX gr' $ oW gr'
        oY gr' = midStartY gr' $ oH gr'
        rs ass gr' = ass { overlayX = oX gr', overlayY = oY gr', overlayW = oW gr', overlayH = oH gr'}
        currFundTxt = T.append "Current Funds: " $ showMoney $ gameDataFunds gd
        costTxt = T.append "Item Cost: " $ showMoney price
        aftTxt = T.append "After Purchase: " $ showMoney (gameDataFunds gd - price)
        stack = AssetStacked $ AssetStack StackVertical [ StackItem (AssetText currFundTxt Green 3) 0 0
                                                        , StackItem (AssetText costTxt Red 3) 0 0
                                                        , StackItem (AssetText aftTxt Green 3) 0 0
                                                        ] 5
        popAssets = M.fromList $ zip [0..]
                                     [ Asset (AssetText name White 9) (oX gr + 100) (oY gr + 60) 1 True $
                                             Just (\ass gr' -> ass { assetX = oX gr' + 100, assetY = oY gr' + 60 })
                                     , Asset stack (oX gr + 150) (oY gr + 200) 1 True $
                                             Just (\ass gr' -> ass { assetX = oX gr' + 150, assetY = oY gr' + 200})
                                     ]
        rsFn ma gr' = ma { menuXBase = oX gr' + 100, menuYBase = oY gr' + oH gr' - 150 }
        menu = DefaultMenu $ MenuAsset (oX gr + 100) (oY gr + oH gr - 150) 2 (Just rsFn) 5
                                       [ MenuItem (MText "Confirm Purchase") White 4 0 0 True (if BuyPopupConfirm == sel then Just LightGray else Nothing) Nothing
                                       , MenuItem (MText "Cancel") White 4 0 0 True (if BuyPopupCancel == sel then Just LightGray else Nothing) Nothing
                                       ]


data EquipMOpt = EquipStoreOpt | EquipReturnLabOpt
               deriving (Show, Eq, Ord, Enum, Bounded)

data EquipManagementState = EquipManagementState (MenuStateInfo EquipMOpt) Int

initEquipManagementState :: GameData -> EquipManagementState
initEquipManagementState gd = EquipManagementState (initMenuStateInfo gd) 0

emsScrollIdx :: Int
emsScrollIdx = 0

instance GamePlayStateE EquipManagementState where
    think gps@(EquipManagementState msi scrPos) cfgs inputs =
        case mouseInputs inputs of
            Just (MouseInputs sAmt) -> stepInputUpdate $ EquipManagementState msi (max 0 (scrPos + 5 * (-sAmt)))
            Nothing -> menuInfoThink mkGPS menuAction back msi inputs
        where
            mkGPS msi' = EquipManagementState msi' scrPos
            gd = menuGetData msi
            back = stepTransition $ initLabTopState gd
            menuAction EquipStoreOpt = stepTransition $ initEquipStoreState gd
            menuAction EquipReturnLabOpt = back

    transition gps@(EquipManagementState msi scrPos) cfgs gr = (gview, [])
        where
            gd = menuGetData msi
            mOpt = menuSel msi
            pSelM = menuGetPause msi
            gview = GView assets (overlays pSelM) $ Just menu
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            overlays _ = mempty
            ownTxt = "Owned Equipment"
            ownX gr' = midTextStart gr' ownTxt $ fromIntegral 4
            assets = M.fromList $ zip [0..]
                                      [ Asset (AssetScroll $ ScrollObj (M.singleton 0 (mkTable gr)) scrPos (scrollH gr)) 10 (scrollYStart gr) 0 True $ Just rs
                                      , staticText "Equipment" White 20 20 8 0
                                      , staticText "Management" White 175 120 8 0
                                      , Asset (AssetText ownTxt LightGray 4) (ownX gr) ((graphicsWindowHeight gr `div` 2) - 100) 0 True $
                                              Just (\ass gr' -> ass { assetX = ownX gr', assetY = (graphicsWindowHeight gr' `div` 2) - 100 })
                                      ]
            scrollYStart gr' = (graphicsWindowHeight gr' `div` 2) + 20
            equipList = gameOwnedEquipment $ gameDataEquipment gd
            equipItems = (!) (equipment $ sharkCfgs cfgs) <$> equipList
            equipRows = (\e -> ([(equipText e, LightGray), (equipTypeText (equipInfoType e), LightGray), (slotsTxt e, LightGray)], 3)) <$> equipItems
            slotsTxt e = T.pack (show (equipSize e)) <> if equipSize e == 1 then " slot" else " slots"
            mkTable gr' = expandingCenterTable gr' (\ass _ -> ass) equipRows 0 0 80 0 15 0
            scrollH gr' =
                let ht = graphicsWindowHeight gr'
                in ht - (ht `div` 2) + 20 - 150
            rs as gr' = as { object = AssetScroll $ ScrollObj (M.singleton 0 (mkTable gr')) 0 (scrollH gr'), assetY = scrollYStart gr' }
            hlM cmp = if cmp == mOpt then Just White else Nothing
            mY gr' = graphicsWindowHeight gr' - 150
            mnRs ass gr' = ass { menuYBase = mY gr' }
            menu = DefaultMenu $ MenuAsset 150 (mY gr) 0 (Just mnRs) 10 items
            items = [ MenuItem (MText "Equipment Store") Blue 3 0 0 True (hlM EquipStoreOpt) Nothing
                    , MenuItem (MText "Back") Blue 3 0 0 True (hlM EquipReturnLabOpt) Nothing
                    ]

    update gps@(EquipManagementState msi scrPos) gvO cfgs gr = menuInfoUpdate newGPS msi gvO gr updateScrollPos
        where
            maxP = maybe 0 (getMaxPosition gr) (getScroll gvO)
            newScrPos = min scrPos maxP
            newGPS msi' = EquipManagementState msi' newScrPos
            getScroll gv = case object (assets gv M.! emsScrollIdx) of
                AssetScroll sc -> Just sc
                _ -> Nothing
            updateScrollPos gv = gv { assets = M.adjust upScroll 3 (assets gv) }
            upScroll as = case object as of
                AssetScroll sc -> as { object = AssetScroll $ updateScrollPosition gr sc newScrPos }
                _ -> as


equipStoreInfo :: GameData -> GameConfigs -> [(T.Text, GameEquipment)]
equipStoreInfo gd cfgs = M.assocs available
    where
        equips = equipment $ sharkCfgs cfgs
        owned = gameOwnedEquipment $ gameDataEquipment gd
        available = M.withoutKeys equips $ S.fromList owned

equipCostTxt :: GameEquipment -> Int -> T.Text
equipCostTxt e maxCost = T.justifyRight width ' ' $ showMoney $ equipPrice e
    where
        width = T.length $ showMoney maxCost

data EquipPopupInfo = EquipPopupInfo
    { equipKey     :: T.Text
    , equipDisplay :: T.Text
    , equipCost    :: Int
    , equipPopSel  :: BuyPopupOpt
    }

data EquipStoreState = EquipStoreState (Either Int EquipPopupInfo) SimpleStateInfo

initEquipStoreState :: GameData -> EquipStoreState
initEquipStoreState gd = EquipStoreState (Left 0) $ SimpleInfo gd Nothing

initEquipPopupInfo :: (T.Text, GameEquipment) -> EquipPopupInfo
initEquipPopupInfo (ek, e) = EquipPopupInfo ek (equipText e) (equipPrice e) minBound

instance GamePlayStateE EquipStoreState where
    think gps@(EquipStoreState equipPopE si) cfgs inputs
        | moveInputJustPressed inputs && not (simpleInfoPaused si) =
            case (inputDirection inputs, equipPopE) of
                (Just DUp, Left 0) -> Step NoChange
                (Just DUp, Left idx) -> stepInputUpdate $ EquipStoreState (Left (idx - 1)) si
                (Just DDown, Left idx) | idx == availEquips -> Step NoChange
                (Just DDown, Left idx) -> stepInputUpdate $ EquipStoreState (Left (idx + 1)) si
                (Just DUp, Right epi@(EquipPopupInfo _ _ _ BuyPopupCancel)) -> stepInputUpdate $ EquipStoreState (Right (epi { equipPopSel = BuyPopupConfirm })) si
                (Just DDown, Right epi@(EquipPopupInfo _ _ _ BuyPopupConfirm)) -> stepInputUpdate $ EquipStoreState (Right (epi { equipPopSel = BuyPopupCancel })) si
                _ -> Step NoChange
        | otherwise = simpleInfoThink (EquipStoreState equipPopE) (enterAction equipPopE) si inputs
        where
            gd = gamedata si
            currFunds = gameDataFunds gd
            getPrice idx = equipPrice $ snd $ eList !! idx
            eList = equipStoreInfo gd cfgs
            availEquips = length eList
            enterAction (Left idx)
                | idx < length eList && getPrice idx > currFunds = Step NoChange
                | idx < length eList = stepInputUpdate $ EquipStoreState (Right (initEquipPopupInfo (eList !! idx))) si
                | otherwise = stepTransition $ initEquipManagementState gd
            enterAction (Right (EquipPopupInfo ek _ _ BuyPopupCancel)) =
                case L.elemIndex ek (fst <$> eList) of
                    Nothing -> stepInputUpdate $ EquipStoreState (Left 0) si
                    Just idx -> stepInputUpdate $ EquipStoreState (Left idx) si
            enterAction (Right (EquipPopupInfo ek _ price BuyPopupConfirm)) =
                let gd' = buyEquipment gd ek price
                in stepInputUpdate $ EquipStoreState (Left 0) $ si { gamedata = gd' }

    transition gps@(EquipStoreState popE si) cfgs gr = (gview, [])
        where
            pSelM = pauseSelM si
            gd = gamedata si
            equips = equipStoreInfo gd cfgs
            numEquips = length equips
            maxCost = maximum $ (equipPrice . snd) <$> equips
            maxPriceStr = T.length $ showMoney maxCost
            headingFS = 3
            listingFS = 3
            yStart = div (graphicsWindowHeight gr) 2
            yRsM ma gr' = ma { menuYBase = div (graphicsWindowHeight gr') 2 }
            currFunds = gameDataFunds gd
            canAfford e = equipPrice e <= currFunds
            isEquipSel i = case popE of
                Left idx -> i == idx
                Right _  -> False
            isLeaveSel = case popE of
                Left idx -> idx == numEquips
                Right _  -> False
            buyColor i e
                | canAfford e && isEquipSel i = White
                | canAfford e                 = Green
                | isEquipSel i                = Red
                | otherwise                   = Gray
            headingRow = (Right [("Equipment", Nothing, False), ("Type", Nothing, False), (T.justifyRight maxPriceStr ' ' "Cost", Nothing, False), (" ", Nothing, False)], headingFS, White, Nothing)
            equipRows = zipWith (\i (_, e) ->
                (Right [(equipText e, Nothing, False), (equipTypeText (equipInfoType e), Nothing, False), (equipCostTxt e maxCost, Nothing, False), ("Buy", Nothing, True)], listingFS, Blue, Just (buyColor i e))
                ) [0..] equips
            leaveRow = (Left "Leave Equipment Store", listingFS, Blue, if isLeaveSel then Just White else Nothing)
            menu = makeTableMenu gr (Just yRsM) (headingRow : equipRows ++ [leaveRow]) 60 60 100 yStart 10 1
            gview = GView assets (overlays' popE) $ Just menu
            assets = M.fromList $ zip [0..]
                                      [ staticText "Equipment" White 20 20 9 0
                                      , staticText "Store" White 80 150 9 0
                                      ]
            overlays (Just pSel) = S.singleton $ pauseOverlay gr pSel
            overlays _ = mempty
            overlays' (Left _) = overlays pSelM
            overlays' (Right epi) = S.insert (buyEquipPopup epi gd gr) $ overlays pSelM


buyEquipPopup :: EquipPopupInfo -> GameData -> Graphics -> Overlay
buyEquipPopup (EquipPopupInfo _ name price sel) gd gr = AOverlay 1 popAssets (Just menu) (oX gr) (oY gr) (oW gr) (oH gr) DarkGray $ Just rs
    where
        oW gr' = max 400 $ floor $ fromIntegral (graphicsWindowWidth gr') * 0.8
        oH gr' = max 300 $ floor $ fromIntegral (graphicsWindowHeight gr') * 0.7
        oX gr' = midStartX gr' $ oW gr'
        oY gr' = midStartY gr' $ oH gr'
        rs ass gr' = ass { overlayX = oX gr', overlayY = oY gr', overlayW = oW gr', overlayH = oH gr' }
        currFundTxt = T.append "Current Funds: " $ showMoney $ gameDataFunds gd
        costTxt = T.append "Item Cost: " $ showMoney price
        aftTxt = T.append "After Purchase: " $ showMoney (gameDataFunds gd - price)
        stack = AssetStacked $ AssetStack StackVertical [ StackItem (AssetText currFundTxt Green 3) 0 0
                                                        , StackItem (AssetText costTxt Red 3) 0 0
                                                        , StackItem (AssetText aftTxt Green 3) 0 0
                                                        ] 5
        popAssets = M.fromList $ zip [0..]
                                     [ Asset (AssetText name White 9) (oX gr + 100) (oY gr + 60) 1 True $
                                             Just (\ass gr' -> ass { assetX = oX gr' + 100, assetY = oY gr' + 60 })
                                     , Asset stack (oX gr + 150) (oY gr + 200) 1 True $
                                             Just (\ass gr' -> ass { assetX = oX gr' + 150, assetY = oY gr' + 200 })
                                     ]
        rsFn ma gr' = ma { menuXBase = oX gr' + 100, menuYBase = oY gr' + oH gr' - 150 }
        menu = DefaultMenu $ MenuAsset (oX gr + 100) (oY gr + oH gr - 150) 2 (Just rsFn) 5
                                       [ MenuItem (MText "Confirm Purchase") White 4 0 0 True (if BuyPopupConfirm == sel then Just LightGray else Nothing) Nothing
                                       , MenuItem (MText "Cancel") White 4 0 0 True (if BuyPopupCancel == sel then Just LightGray else Nothing) Nothing
                                       ]
