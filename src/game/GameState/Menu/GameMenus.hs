{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.GameMenus
    ( SplashState(..)
    , MainMenuState(..)
    , MainMenuOpt(..)
    , IntroState(..)
    , IntroPage(..)
    , initSplash
    , initMainMenu
    , initResearchCenter
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)

import Configs
import SaveData
import Shark.Trip
import Shark.Types

import OutputHandles.Types
    ( Color(..)
    , OutputHandles(textures)
    , TextDisplay(TextDisplay)
    )
import OutputHandles.Util

import GameState.Types
import GameState.Util
import Graphics.Types
import Graphics.TextUtil
import Graphics.Asset

import InputState
import Util

import Debug.Trace

data SplashState = SplashState Int64


initSplash :: InputState -> SplashState
initSplash inputs = SplashState (timestamp inputs)

instance GamePlayStateE SplashState where
    think gps@(SplashState start) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | timestamp inputs - start < 1000 = Step NoChange
        | otherwise =
            case lastSaveM (stateCfgs cfgs) of
                Nothing -> Step $ Transition $ AnyGamePlayState $ MainMenuState Nothing NewGameMain []
                Just fp -> LoadSave fp $ \gd -> Transition $ AnyGamePlayState $ MainMenuState (Just gd) ContinueMain []


data MainMenuOpt = ContinueMain | NewGameMain | ExitMain
                  deriving (Enum, Ord, Eq, Show, Bounded)

data MainMenuState = MainMenuState (Maybe GameData) MainMenuOpt [AnimationState]

initMainMenu :: Maybe GameData -> MainMenuState
initMainMenu gdM = MainMenuState gdM minBound []

animKey :: T.Text
animKey = "wave"

defaultGView :: Bool -> [(Int, Asset)] -> GameConfigs -> Graphics -> GView
defaultGView hasSave waveAssets cfgs gr = gview
    where
        gview = GView (M.fromList assets) mempty $ Just $ menuAsset (menuX gr) (menuY gr)
        assets = (zip [(length waveAssets)..]
                    [ back
                    , staticText "Shark" Gray 10 10 14 2
                    , staticText "Institute" Gray 100 200 12 2
                    , centerText gr "Press ENTER to select" White 0 20 3 2
                    ]) ++ waveAssets
        back = backgroundAsset gr DarkBlue
        menuOpts = (if hasSave then ["Continue"] else []) ++ ["New Game","Exit"]
        items = mkMenuItem <$> menuOpts
        mkMenuItem i = MenuItem (MText i) Blue 3 0 0 True Nothing Nothing
        menuAsset x y = DefaultMenu $ MenuAsset x y 2 (Just resizeMenu) 2 items
        midX gr' = graphicsWindowWidth gr' `div` 2
        midY gr' = graphicsWindowHeight gr' `div` 2
        menuX gr' = midX gr' - 40
        menuY gr' = midY gr' + 100
        resizeMenu asset' gr' = asset' { menuXBase = menuX gr', menuYBase = menuY gr' }

updateWaveAsset :: Int -> Asset -> Graphics -> Asset
updateWaveAsset i an@(Asset _ _ _ _ _ rsM) gr = updateWaveAsset' $ maybe an (\rFn -> rFn an gr) rsM
    where
        updateWaveAsset' anAss@(Asset wa@(AssetAnimation wave f d s) x y l isVis _)
            | newX < graphicsWindowWidth gr = anAss { assetX = newX, isVisible = show, object = (AssetAnimation wave nextFrame d s) }
            | mod (f + nextFrame) 3 == 0 = anAss { isVisible = False, object = (AssetAnimation wave nextFrame d s) }
            | otherwise = anAss { assetX = xMod + f, assetY = newY, isVisible = True, object = (AssetAnimation wave 1 d s) }
            where
                maxWaveFrame = animFrameCount $ graphicsAnimTextures gr M.! wave
                nextFrame = (f + 1) `mod` maxWaveFrame
                xMod = i * 2 + round (2 * s)
                newX = if isVis then x + 30 + xMod else x + (xMod * 4) + 100
                newY = max (100 + f * 10) $ (y + 50) `mod` (graphicsWindowHeight gr - 100)
                show
                    | nextFrame == 0 && isVis = False
                    | nextFrame == 0 && f `mod` 3 == 0 = False
                    | otherwise = True

initWaves :: Graphics -> ([AnimationState], [(Int, Asset)])
initWaves gr = (wAnimStates, waveAssets)
    where
        rsFnM = Just waveResize
        waveResize asset gr = asset { assetY = assetY asset `mod` max 1 (graphicsWindowHeight gr - 100) }
        waveAnim = Asset (AssetAnimation animKey 0 0 10.0) 340 400 1 False rsFnM
        waveAnim2 = Asset (AssetAnimation animKey 4 0 8.0) 500 900 1 True rsFnM
        waveAnim3 = Asset (AssetAnimation animKey 7 0 6.0) 100 750 1 True rsFnM
        waveAnim4 = Asset (AssetAnimation animKey 5 0 7.0) 700 200 1 True rsFnM
        waveAnim5 = Asset (AssetAnimation animKey 2 0 5.5) 1100 150 1 True rsFnM
        waveAssets = zip [0..] [waveAnim, waveAnim2, waveAnim3, waveAnim4, waveAnim5]
        wAnimStates = animState <$> waveAssets
        animState (idx, wave) = AnimState 0 120 [idx] $ animFn idx
        animFn idx pWave gr = updateWaveAsset (idx + 1) pWave gr

applyMainOptState :: MainMenuState -> GView -> GView
applyMainOptState (MainMenuState gdM mmo _) gv = gv { menuAsset = updateM $ menuAsset gv }
    where
        updateM (Just (DefaultMenu m)) = Just $ DefaultMenu $ changeCursor m ((fromEnum mmo) + cntOff) "green_arrow" 4.0
        updateM _ = Nothing
        gdCnt Nothing = 1
        gdCnt (Just _) = 0
        cntOff = gdCnt gdM


instance GamePlayStateE MainMenuState where
    think gps@(MainMenuState gdM mmo animS) cfgs inputs
        | wasWindowResized inputs = Step ResizeWindow
        | enterJustPressed inputs =
            case (gdM, mmo) of
                (Just gd, ContinueMain) -> Step $ Transition $ AnyGamePlayState $ initResearchCenter gd
                (_, NewGameMain) -> NewGame (\gd -> Transition (AnyGamePlayState (IntroState gd IntroWelcomePage)))
                (_, ExitMain) -> Exit Nothing
                _ -> error "Can't continue non-existant game. What did I do?"
        | moveInputJustPressed inputs =
            case (inputDirection inputs, gdM, mmo) of
                (Just DUp, Just _, ContinueMain) -> Step NoChange
                (Just DUp, Nothing, NewGameMain) -> Step NoChange
                (Just DUp, _, _) -> stepInputUpdate $ MainMenuState gdM (pred mmo) animS
                (Just DDown, _, ExitMain) -> Step NoChange
                (Just DDown, _, _) -> stepInputUpdate $ MainMenuState gdM (succ mmo) animS
                _ -> Step NoChange
        | otherwise = Step $ getAnimationStep inputs animS

    transition gps@(MainMenuState gdM mmo _) cfgs gr = (applyMainOptState gps gview, waves)
        where
            (waves, wAssets) = initWaves gr
            gview = defaultGView (isJust gdM) wAssets cfgs gr
    update gps gv cfgs gr = GameState (AnyGamePlayState gps) $ applyMainOptState gps gv

    updateAnims (MainMenuState gdM mmo _) newAnims = MainMenuState gdM mmo newAnims


data IntroPage = IntroWelcomePage | IntroMissionPage | IntroBoatPage | IntroEquipPage | IntroResearchPage | IntroFundsPage | IntroEndPage
               deriving (Show, Enum, Eq, Ord, Bounded)

data IntroState = IntroState GameData IntroPage

instance GamePlayStateE IntroState where
    think is@(IntroState gd page) _ inputs
        | wasWindowResized inputs = Step ResizeWindow
        | enterJustPressed inputs && page < IntroEndPage = Step $ Transition $ AnyGamePlayState $ IntroState gd $ succ page -- maybe transition?
        | enterJustPressed inputs && page == maxBound = Step $ Transition $ AnyGamePlayState $ initResearchCenter gd
        | otherwise = Step NoChange

    transition is@(IntroState gd IntroWelcomePage) _ gr = introWelcome gd gr
    transition is@(IntroState gd IntroMissionPage) _ gr = introMission gd gr
    transition is@(IntroState gd IntroBoatPage) cfgs gr = introBoat gd cfgs gr
    transition is@(IntroState gd IntroEquipPage) cfgs gr = introEquip gd cfgs gr
    transition is@(IntroState gd IntroResearchPage) _ gr = introResearch gd gr
    transition is@(IntroState gd IntroFundsPage) _ gr = introFunds gd gr
    transition is@(IntroState gd IntroEndPage) _ gr = introEnd gd gr


introWelcome :: GameData -> Graphics -> (GView, [AnimationState])
introWelcome gd gr = (GView (M.fromList assets) mempty $ Just $ nextMenu gr 1, [])
        where
            assets = zip [0..]
                         [ centerTextX gr "Welcome!" White 12 0 20 1
                         , withRow
                         , centerTextX gr endText Green 2 0 600 1
                         ]
            welcomeAsset = wrapTextAsset gr 90 White welcomeText 2 3 1 200 False
            dolphImg = AssetImage "no_dolphin" 1.0
            sharkTxt = AssetText "SHARKS!!!" White 4
            sharkRow x _ gr' = AssetStacked $ AssetStack StackHorizontal
                                [ StackItem dolphImg  (150 - x) 20
                                , StackItem sharkTxt (getCenterX gr' (x + assetObjWidth gr' dolphImg) (assetObjWidth gr' sharkTxt)) 80
                                ] 0
            withRow = appendAssetStackWith gr StackVertical welcomeAsset sharkRow
            welcomeText = "You and a group of shark enthusiasts / scientists want to learn more about sharks instead of \
                        \those marine mammals that seem to dominate the marine biology departments. \
                        \Together you decide to open a new research center dedicated entirely to"
            endText = "And you have been appointed the new director of the center!"

introMission :: GameData -> Graphics -> (GView, [AnimationState])
introMission gd gr = (GView (M.fromList assets) mempty $ Just $ nextMenu gr 1, [])
    where
        assets = zip [0..]
                     [ staticText "Mission" White 50 20 9 0
                     , staticText "Statement" White 150 150 9 0
                     , wrapTextAsset gr 90 White howItWorksText 2 3 0 300 False
                     ]
        howItWorksText = "As the director of the Shark Research Center, you will plan research trips \
                         \to find a variety of shark species in the wild, collect data about them, \
                         \and contribute to the scientific and the community's understanding of these \
                         \fascinating creatures by publishing your findings. By publishing new papers \
                         \the institute will gain recognition and funding to continue its important work. \
                         \You will need to manage your resources, plan research expeditions, and publish the data collected \
                         \to contribute to the understanding of sharks."

introBoat :: GameData -> GameConfigs -> Graphics -> (GView, [AnimationState])
introBoat gd cfg gr = (GView (M.fromList assets) mempty $ Just $ nextMenu gr 1, [])
    where
        startBoatKey = gameActiveBoat $ gameDataEquipment gd
        startBoat = boats (sharkCfgs cfg) M.! startBoatKey
        assets = zip [0..]
                     [ staticText "A Kind" White 40 20 8 0
                     , staticText "Neighbor" White 100 130 9 0
                     , wrapTextAsset gr 85 White boatText 2 3 0 320 False
                     , centerAssetX gr (AssetImage (boatImage startBoat) 2.0) 100 400 0
                     ]
        boatText = "Luckily a retiring fisherman has decided to donate his skiff to your cause, \
                   \so you have a way to conduct your research trips. It might be old and small and smelly \
                   \but it floats (most of the time). Maybe in the future we can get another boat..."

introEquip :: GameData -> GameConfigs -> Graphics -> (GView, [AnimationState])
introEquip gd cfg gr = (GView (M.fromList assets) mempty $ Just $ nextMenu gr 1, [])
    where
        eqKeys = gameOwnedEquipment $ gameDataEquipment gd
        eqs = map (\k -> equipment (sharkCfgs cfg) M.! k) eqKeys
        eqCatch = head $ filter (\e -> equipInfoType e == Caught) eqs
        eqObs = head $ filter (\e -> equipInfoType e == Observed) eqs
        equipmentText = "With the initial funds you were also able to purchase some basic research equipment \
                        \to get your center up and running. It's not much, but it's a start. \
                        \Hopefully as you make progress in your research you can acquire more advanced gear."
        rectStartY = 180
        assets = zip [0..]
                     [ staticText "Equipment" White 30 20 9 0
                     , centerAssetX gr (AssetRect 600 300 LightGray) 0 rectStartY 0
                     , mkResizeAsset gr (AssetText "Catch" Blue 3) 1 True resizeCatch
                     , mkResizeAsset gr (AssetText "Observe" Blue 3) 1 True resizeObserve
                     , mkResizeAsset gr (AssetImage (equipImage eqCatch) 4.0) 1 True resizeCatchImg
                     , mkResizeAsset gr (AssetImage (equipImage eqObs) 4.0) 1 True resizeObserveImg
                     , wrapTextAsset gr 95 White equipmentText 2 3 0 500 False
                     ]
        resizeCatch asset' gr' = asset' { assetX = (graphicsWindowWidth gr' `div` 2) - 260, assetY = rectStartY + 10 }
        resizeObserve asset' gr' = asset' { assetX = (graphicsWindowWidth gr' `div` 2) + 40, assetY = rectStartY + 10 }
        resizeCatchImg asset' gr' = asset' { assetX = (graphicsWindowWidth gr' `div` 2) - 290, assetY = rectStartY + 50 }
        resizeObserveImg asset' gr' = asset' { assetX = (graphicsWindowWidth gr' `div` 2) + 45, assetY = rectStartY + 50 }

introResearch :: GameData -> Graphics -> (GView, [AnimationState])
introResearch gd gr = (GView (M.fromList assets) mempty $ Just $ nextMenu gr 1, [])
    where
        researchText = "The type of data you gather from each shark will depend on the type of equipment you used to \
                       \collect it. Using catch equipment will allow you to gather physical data such as size, weight, \
                       \and health indicators, while observation equipment will enable you to record behavioral data \
                       \such as feeding habits, social interactions, and movement patterns. As you discover more sharks \
                       \and publish more papers you will be inspired to start working on new research topics. Check back \
                       \at the research center to see what data you will need to collect to finish these new studies. \
                       \Once your next paper is published you are sure to be rolling in the grant money!"
        assets = zip [0..]
                     [ staticText "Publishing" White 30 20 9 0
                     , staticText "Your Findings" White 80 150 8 0
                     , wrapTextAsset gr 95 White researchText 2 3 0 300 False
                     ]

introFunds :: GameData -> Graphics -> (GView, [AnimationState])
introFunds gd gr = (GView (M.fromList assets) mempty $ Just $ nextMenu gr 1, [])
    where
        assets = zip [0..]
                     [ staticText "Funds" White 50 30 10 0
                     , withStartMoney
                     ]
        welcomeText = "All together we were able to raise initial funds to \
                      \support our research initiatives. These funds will help \
                      \cover the costs of equipment, travel, and other essential expenses. \
                      \Don't forget to check for new boats and equipment under the lab management menu!"
        grantText = "Initial Funds: "
        startMoney = showMoney $ gameDataFunds gd
        textAsset = wrapTextAsset gr 85 White welcomeText 2 3 0 250 False
        withGrantText = appendAssetStackCenterX gr StackVertical textAsset (-50) 90 $ AssetText grantText White 4
        withStartMoney = appendAssetStackCenterX gr StackVertical withGrantText (-5) 30 $ AssetText startMoney Green 3


introEnd :: GameData -> Graphics -> (GView, [AnimationState])
introEnd gd gr = (GView (M.fromList assets) mempty $ Just menu, [])
    where
        menu = singleMenu gr "Begin Research" 50 1
        assets = zip [0..]
                     [ staticText "Good Luck!" White 100 50 12 0
                     , wrapTextAsset gr 80  White endText 2 3 0 300 False
                     ]
        endText = "With everything in place, you are now ready to begin your research expeditions. \
                  \Good luck, and may your studies contribute significantly to the understanding and conservation of sharks!"

data RCMenu = PlanTripMenu | ReviewDataMenu | LabManagementMenu
             deriving (Show, Eq, Ord, Enum, Bounded)

data ResearchCenterState = ResearchCenterState
    { rcMenuInfo :: MenuStateInfo RCMenu
    , rcAnimations :: [AnimationState]
    }

initResearchCenter :: GameData -> ResearchCenterState
initResearchCenter gd = ResearchCenterState (initMenuStateInfo gd) []

instituteKey :: T.Text
instituteKey = "institute"

flagAnimIdx :: Int
flagAnimIdx = 0

defaultRCView :: GameData -> Graphics -> GView
defaultRCView gd gr = GView (M.fromList assets) mempty $ Just menu
    where
        assets = zip [flagAnimIdx..]
                     [ flagAnim gr
                     , staticText "Research" White 80 10 7 0
                     , staticText "Center" White 240 140 7 0
                     , oneLineTextRs (\ass gr' -> ass { assetX = graphicsWindowWidth gr' `div` 2 })
                                     [("Current Funds: ", White, 3), (showMoney funds, Green, 3)] 2 (graphicsWindowWidth gr `div` 2) 100 0
                     , imgAsset gr
                     ]
        funds = gameDataFunds gd
        menu = DefaultMenu $ MenuAsset 100 400 1 Nothing 15 mItems
        mItems = [ MenuItem (MText "Plan Research Trip") Blue 3 0 0 True Nothing Nothing
                 , MenuItem (MText "Review Data") Blue 3 0 0 True Nothing Nothing
                 , MenuItem (MText "Lab Management") Blue 3 0 0 True Nothing Nothing
                 ]
        imgScale gr' = max 2.0 $ min ((fromIntegral (graphicsWindowWidth gr') / 2 - 40) / fromIntegral (instituteW gr'))
                                     ((fromIntegral (graphicsWindowHeight gr') - 270) / fromIntegral (instituteH gr'))
        instituteInfo gr' = graphicsStaticTextures gr' M.! "institute"
        instituteW gr' = imageSizeX $ instituteInfo gr'
        instituteH gr' = imageSizeY $ instituteInfo gr'
        imgX gr' = graphicsWindowWidth gr' `div` 2
        imgY = 250
        imgAsset gr' = Asset (AssetImage "institute" (imgScale gr')) (imgX gr') imgY 1 True $ Just rsImg
        rsImg as gr' = as { object = AssetImage instituteKey (max 2.0 (imgScale gr')), assetX = graphicsWindowWidth gr' `div` 2 }
        flagAnim gr' =
            let scale = imgScale gr'
            in Asset (AssetAnimation "flag_with_shadow" 0 0 scale) (imgX gr' + floor (21 * scale)) (imgY + floor (132 * scale)) 2 True (Just flagRs)
        flagRs asset' gr' =
            let scale = imgScale gr'
            in asset' { object = AssetAnimation "flag_with_shadow" 0 0 scale, assetX = imgX gr' + floor (21 * scale), assetY = imgY + floor (132 * scale) }

initFlagAnimState :: AnimationState
initFlagAnimState = AnimState 0 170 [flagAnimIdx] anUp
    where
        anUp as@(Asset (AssetAnimation img f d scale) _ _ _ _ rs) gr' =
            let as' = maybe as (\rsF -> rsF as gr') rs
                newF = mod (f + 1) $ animFrameCount $ graphicsAnimTextures gr' M.! img
            in as' { object = AssetAnimation img newF d scale}

applyRCState :: ResearchCenterState -> GView -> Graphics -> GView
applyRCState rcs gv gr = menuInfoApply (rcMenuInfo rcs) gv gr id

instance GamePlayStateE ResearchCenterState where
    think rcs@(ResearchCenterState msi as) _ inputs =
        case menuInfoThink mkRCS selNext msi inputs of
            Step NoChange -> Step $ getAnimationStep inputs as
            act -> act
        where
            gd = gamedata $ stateInfo msi
            selNext PlanTripMenu = Step $ TopTransition TripMenus gd
            selNext ReviewDataMenu = Step $ TopTransition ReviewMenus gd
            selNext LabManagementMenu = Step $ TopTransition LabMenus gd
            mkRCS msi' = ResearchCenterState msi' as


    transition rcs _ gr = (applyRCState rcs baseGV gr, [initFlagAnimState])
        where
            gd = gamedata $ stateInfo $ rcMenuInfo rcs
            baseGV = defaultRCView gd gr

    update gps gv _ gr = GameState (AnyGamePlayState gps) $ applyRCState gps gv gr

    updateAnims gps anims = gps { rcAnimations = anims }
