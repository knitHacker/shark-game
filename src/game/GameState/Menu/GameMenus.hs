{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.GameMenus
    ( initialMainMenuState
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Configs
import SaveData
import Shark.Types

import OutputHandles.Types ( Color(..), TextDisplay(TextDisplay) )

import GameState.Types
import GameState.Helpers
import Graphics.Types hiding (updateMenu)
import Graphics.Menu
import Graphics.TextUtil
import Graphics.ImageUtil
import Graphics.Animation

import InputState
import Data.Int (Int64)


-- ===========================================================================
-- MainMenuState
-- ===========================================================================

updateWave :: Graphics -> Int -> (Int, AnimPlacement) -> AnimPlacement
updateWave gr fr (i, wave)
    | newX < graphicsWindowWidth gr = wave { animPosX = newX, animShow = show', animFrame = nextFrame }
    | mod (fr + nextFrame) 3 == 0   = wave { animShow = False }
    | otherwise = wave { animPosX = xMod + fr, animPosY = newY, animShow = True, animFrame = 1 }
    where
        maxWaveFrame = animFrameCount $ graphicsAnimTextures gr M.! animTexture wave
        nextFrame = (animFrame wave + 1) `mod` maxWaveFrame
        xMod = i * 2 + round (2 * animScale wave)
        newX = if animShow wave then animPosX wave + 30 + xMod else animPosX wave + (xMod * 4) + 100
        newY = max (100 + fr * 10) $ (animPosY wave + 50) `mod` (graphicsWindowHeight gr - 100)
        show'
            | nextFrame == 0 && animShow wave  = False
            | nextFrame == 0 && fr `mod` 3 == 0 = False
            | otherwise                          = True


initialWaves :: [AnimPlacement]
initialWaves =
    [ APlace 340  400 10.0 "wave" 0 0 1 False
    , APlace 500  900  8.0 "wave" 4 0 1 True
    , APlace 100  750  6.0 "wave" 7 0 1 True
    , APlace 700  200  7.0 "wave" 5 0 1 True
    , APlace 1100 150  5.5 "wave" 2 0 1 True
    ]


waveAnimPeriod :: Int64
waveAnimPeriod = 120

waveFrameCount :: Int
waveFrameCount = 15

data MainMenuState = MainMenuState
    { mainMenuGameData :: !(Maybe GameData)
    , mainMenuWaves    :: ![AnimPlacement]
    , mainMenuFrame    :: !Int
    , mainMenuLastAnim :: !Int64
    , mainMenuCursor   :: !Int
    }


mainMenuOpts :: Maybe GameData -> [MenuAction Update]
mainMenuOpts gdM =
    let newGame         = MenuAction "New Game"  Nothing $ Just $ GenerateNewGame $ \gd -> Transition $ AnyGamePlayState (IntroState IntroWelcome gd) False
        continueGame gd = MenuAction "Continue"  Nothing $ Just $ PureStep $ Transition $ AnyGamePlayState (ResearchCenterState gd 0 0 0) True
        exitOpt         = MenuAction "Exit"       Nothing $ Just Exit
    in case gdM of
        Nothing -> [newGame, exitOpt]
        Just gd -> [continueGame gd, newGame, exitOpt]


mkMainMenu :: Maybe GameData -> Graphics -> Int -> ViewMenu
mkMainMenu gdM gr cursor =
    let midY    = div (graphicsWindowHeight gr) 2
        instTxt = "Press Enter to select" :: T.Text
        xPos    = midTextStart gr instTxt 3
        md = selOneOpts (xPos + 100) (midY + 100) 3 2 (mainMenuOpts gdM) Nothing (CursorPointer "green_arrow") cursor
    in ViewMenu $ Menu md Nothing


instance GamePlayState MainMenuState where
    think s@(MainMenuState gdM waves frame lastAnim cursor) _ inputRes gr =
        let inputs    = newInputs inputRes
            animTick  = timestamp inputs - lastAnim >= waveAnimPeriod
            frame'    = if animTick then (frame + 1) `mod` waveFrameCount else frame
            waves'    = if animTick then updateWave gr frame' <$> zip [1..] waves else waves
            lastAnim' = if animTick then timestamp inputs else lastAnim
            s'        = s { mainMenuWaves = waves', mainMenuFrame = frame', mainMenuLastAnim = lastAnim' }
            ViewMenu m = mkMainMenu gdM gr cursor
        in case updateMenu inputs m of
            Just (Right upd) -> upd
            Just (Left m')   -> PureStep $ Refresh $ AnyGamePlayState
                                    (s' { mainMenuCursor = cursorPosition (options m') }) True
            Nothing
                | animTick || windowResized inputRes /= Nothing ->
                    PureStep $ Refresh $ AnyGamePlayState s' True
                | otherwise -> PureStep UseCache

    draw (MainMenuState gdM waves _ _ cursor) gr _ =
        let midY     = div (graphicsWindowHeight gr) 2
            instTxt  = "Press Enter to select" :: T.Text
            (instr, _) = textMiddleX gr instTxt (midY + 20) 3 White
            rect     = RPlace DarkBlue 0 0 (graphicsWindowWidth gr) (graphicsWindowHeight gr) 0
            v        = View wordLayers [] waves [rect] Nothing
            wordLayers =
                [ (TextDisplay "Shark"     10  10  14 Gray Nothing, 2)
                , (TextDisplay "Institute" 100 200 12 Gray Nothing, 2)
                , (instr, 2)
                ]
        in GameView v Nothing (Just $ mkMainMenu gdM gr cursor)


initialMainMenuState :: Maybe GameData -> AnyGamePlayState
initialMainMenuState gdM = AnyGamePlayState (MainMenuState gdM initialWaves 0 0 0) True


-- ===========================================================================
-- Intro sequence
-- ===========================================================================

data IntroPage
    = IntroWelcome
    | IntroMission
    | IntroBoat
    | IntroEquipment
    | IntroResearch
    | IntroFunds
    | IntroEnd

data IntroState = IntroState
    { introPage     :: !IntroPage
    , introGameData :: !GameData
    }

nextIntroState :: IntroPage -> GameData -> AnyGamePlayState
nextIntroState IntroWelcome   gd = AnyGamePlayState (IntroState IntroMission   gd) False
nextIntroState IntroMission   gd = AnyGamePlayState (IntroState IntroBoat      gd) False
nextIntroState IntroBoat      gd = AnyGamePlayState (IntroState IntroEquipment gd) False
nextIntroState IntroEquipment gd = AnyGamePlayState (IntroState IntroResearch  gd) False
nextIntroState IntroResearch  gd = AnyGamePlayState (IntroState IntroFunds     gd) False
nextIntroState IntroFunds     gd = AnyGamePlayState (IntroState IntroEnd       gd) False
nextIntroState IntroEnd       gd = AnyGamePlayState (ResearchCenterState gd 0 0 0) True

instance GamePlayState IntroState where
    think (IntroState page gd) _ inputRes _ =
        let inputs = newInputs inputRes
        in if enterJustPressed inputs
           then PureStep $ Transition $ nextIntroState page gd
           else if windowResized inputRes /= Nothing
                then PureStep $ Refresh $ AnyGamePlayState (IntroState page gd) False
                else PureStep UseCache

    draw (IntroState page gd) gr cfgs = drawIntroPage page gd gr cfgs


introNextOpt :: IntroPage -> GameData -> MenuAction Update
introNextOpt IntroEnd gd = MenuAction "Begin Research" Nothing $ Just $ PureStep $ Transition $ nextIntroState IntroEnd gd
introNextOpt page     gd = MenuAction "Next"           Nothing $ Just $ PureStep $ Transition $ nextIntroState page    gd

introNextMenu :: IntroPage -> GameData -> Graphics -> ViewMenu
introNextMenu page gd gr =
    let optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100
    in ViewMenu $ Menu (selOneOpts optX optY 3 2 [introNextOpt page gd] Nothing (CursorRect White) 0) Nothing

drawIntroPage :: IntroPage -> GameData -> Graphics -> GameConfigs -> GameView
drawIntroPage IntroWelcome   gd gr _    = drawIntroWelcome   gd gr
drawIntroPage IntroMission   gd gr _    = drawIntroMission   gd gr
drawIntroPage IntroBoat      gd gr cfgs = drawIntroBoat      gd gr cfgs
drawIntroPage IntroEquipment gd gr cfgs = drawIntroEquipment gd gr cfgs
drawIntroPage IntroResearch  gd gr _    = drawIntroResearch  gd gr
drawIntroPage IntroFunds     gd gr _    = drawIntroFunds     gd gr
drawIntroPage IntroEnd       gd gr _    = drawIntroEnd       gd gr


drawIntroWelcome :: GameData -> Graphics -> GameView
drawIntroWelcome gd gr =
    let welcomeText = "You and a group of shark enthusiasts / scientists want to learn more about sharks instead of \
                      \those marine mammals that seem to dominate the marine biology departments. \
                      \Together you decide to open a new research center dedicated entirely to"
        endText = "And you have been appointed the new director of the center!"
        (wrappedWelcome, yEnd) = wrapTextMiddleX gr welcomeText 50 200 3 2 White
        words = [ fst $ textMiddleX gr "Welcome!" 20 12 White
                ] ++ wrappedWelcome ++
                [ fst $ textMiddleX gr "SHARKS!!!" (fromIntegral yEnd + 100) 4 White
                , fst $ textMiddleX gr endText 600 2 Green
                ]
        imgs = [IPlace 150 (yEnd + 20) 1.0 "no_dolphin" 1]
        v = View ((,0) <$> words) imgs [] [] Nothing
    in GameView v Nothing (Just $ introNextMenu IntroWelcome gd gr)

drawIntroMission :: GameData -> Graphics -> GameView
drawIntroMission gd gr =
    let howItWorksText = "As the director of the Shark Research Center, you will plan research trips \
                         \to find a variety of shark species in the wild, collect data about them, \
                         \and contribute to the scientific and the community's understanding of these \
                         \fascinating creatures by publishing your findings. By publishing new papers \
                         \the institute will gain recognition and funding to continue its important work. \
                         \You will need to manage your resources, plan research expeditions, and publish the data collected \
                         \to contribute to the understanding of sharks."
        (wrappedHowItWorks, _) = wrapTextMiddleX gr howItWorksText 40 300 3 2 White
        words = TextDisplay "Mission" 50 20 9 White Nothing
              : TextDisplay "Statement" 150 150 9 White Nothing
              : wrappedHowItWorks
    in GameView (textView words) Nothing (Just $ introNextMenu IntroMission gd gr)

drawIntroBoat :: GameData -> Graphics -> GameConfigs -> GameView
drawIntroBoat gd gr cfgs =
    let startBoatKey = gameActiveBoat $ gameDataEquipment gd
        startBoat = boats (sharkCfgs cfgs) M.! startBoatKey
        img = IPlace 500 400 2.0 (boatImage startBoat) 1
        boatText = "Luckily a retiring fisherman has decided to donate his skiff to your cause, \
                   \so you have a way to conduct your research trips. It might be old and small and smelly \
                   \but it floats (most of the time). Maybe in the future we can get another boat..."
        (wrappedBoat, _) = wrapTextMiddleX gr boatText 75 320 3 2 White
        words = TextDisplay "A Kind" 40 20 8 White Nothing
              : TextDisplay "Neighbor" 100 130 9 White Nothing
              : wrappedBoat
        v = View ((,0) <$> words) [img] [] [] Nothing
    in GameView v Nothing (Just $ introNextMenu IntroBoat gd gr)

drawIntroEquipment :: GameData -> Graphics -> GameConfigs -> GameView
drawIntroEquipment gd gr cfgs =
    let eqKeys = gameOwnedEquipment $ gameDataEquipment gd
        eqs = map (\k -> equipment (sharkCfgs cfgs) M.! k) eqKeys
        eqCatch = head $ filter (\e -> equipInfoType e == Caught) eqs
        eqObs   = head $ filter (\e -> equipInfoType e == Observed) eqs
        equipmentText = "With the initial funds you were also able to purchase some basic research equipment \
                        \to get your center up and running. It's not much, but it's a start. \
                        \Hopefully as you make progress in your research you can acquire more advanced gear."
        (wrappedEquip, _) = wrapTextMiddleX gr equipmentText 70 500 3 2 White
        rectStartX = midStartX gr 600
        rectStartY = 180
        words = (TextDisplay "Equipment" 30 20 9 White Nothing, 0)
              : (TextDisplay "Catch"   (fromIntegral rectStartX + 40)  (fromIntegral rectStartY + 10) 3 Blue Nothing, 2)
              : (TextDisplay "Observe" (fromIntegral rectStartX + 340) (fromIntegral rectStartY + 10) 3 Blue Nothing, 2)
              : ((,0) <$> wrappedEquip)
        imgs  = [ IPlace (rectStartX + 10)  (rectStartY + 50) 4.0 (equipImage eqCatch) 2
                , IPlace (rectStartX + 345) (rectStartY + 50) 4.0 (equipImage eqObs)   2
                ]
        rects = [ RPlace LightGray rectStartX rectStartY 600 300 1 ]
        v = View words imgs [] rects Nothing
    in GameView v Nothing (Just $ introNextMenu IntroEquipment gd gr)

drawIntroResearch :: GameData -> Graphics -> GameView
drawIntroResearch gd gr =
    let researchText = "The type of data you gather from each shark will depend on the type of equipment you used to \
                       \collect it. Using catch equipment will allow you to gather physical data such as size, weight, \
                       \and health indicators, while observation equipment will enable you to record behavioral data \
                       \such as feeding habits, social interactions, and movement patterns. As you discover more sharks \
                       \and publish more papers you will be inspired to start working on new research topics. Check back \
                       \at the research center to see what data you will need to collect to finish these new studies. \
                       \Once your next paper is published you are sure to be rolling in the grant money!"
        (wrappedResearch, _) = wrapTextMiddleX gr researchText 50 300 4 2 White
        words = TextDisplay "Publishing" 30 20 9 White Nothing
              : TextDisplay "Your Findings" 80 150 8 White Nothing
              : wrappedResearch
    in GameView (textView words) Nothing (Just $ introNextMenu IntroResearch gd gr)

drawIntroFunds :: GameData -> Graphics -> GameView
drawIntroFunds gd gr =
    let welcomeText = "All together we were able to raise initial funds to \
                      \support our research initiatives. These funds will help \
                      \cover the costs of equipment, travel, and other essential expenses. \
                      \Don't forget to check for new boats and equipment under the lab management menu!"
        startMoney = gameDataFunds gd
        (wrappedWelcome, yEnd) = wrapTextMiddleX gr welcomeText 50 250 3 2 White
        words = TextDisplay "Funds" 50 30 10 White Nothing
              : TextDisplay "Initial Funds: " 300 (fromIntegral yEnd + 100) 4 White Nothing
              : TextDisplay (showMoney startMoney) 500 600 3 Green Nothing
              : wrappedWelcome
    in GameView (textView words) Nothing (Just $ introNextMenu IntroFunds gd gr)

drawIntroEnd :: GameData -> Graphics -> GameView
drawIntroEnd gd gr =
    let endText = "With everything in place, you are now ready to begin your research expeditions. \
                  \Good luck, and may your studies contribute significantly to the understanding and conservation of sharks!"
        (wrappedEnd, _) = wrapTextMiddleX gr endText 100 300 3 2 White
        words = TextDisplay "Good Luck!" 100 50 12 White Nothing : wrappedEnd
    in GameView (textView words) Nothing (Just $ introNextMenu IntroEnd gd gr)


-- ===========================================================================
-- ResearchCenterState
-- ===========================================================================

rcAnimPeriod :: Int64
rcAnimPeriod = 170

data ResearchCenterState = ResearchCenterState
    { rcGameData  :: !GameData
    , rcFlagFrame :: !Int
    , rcLastAnim  :: !Int64
    , rcCursor    :: !Int
    }

researchCenterOpts :: GameData -> [MenuAction Update]
researchCenterOpts _ =
    [ MenuAction "Plan Research Trip" Nothing $ Just $ PureStep UseCache
    , MenuAction "Review Data"        Nothing $ Just $ PureStep UseCache
    , MenuAction "Lab Management"     Nothing $ Just $ PureStep UseCache
    ]

mkResearchCenterMenu :: GameData -> Int -> ViewMenu
mkResearchCenterMenu gd cursor =
    ViewMenu $ Menu (selOneOpts 100 400 3 15 (researchCenterOpts gd) Nothing (CursorRect White) cursor) Nothing

instance GamePlayState ResearchCenterState where
    think s@(ResearchCenterState gd flagFrame lastAnim cursor) _ inputRes gr =
        let inputs    = newInputs inputRes
            animTick  = timestamp inputs - lastAnim >= rcAnimPeriod
            maxFrame  = animFrameCount $ graphicsAnimTextures gr M.! "flag_with_shadow"
            flagFrame' = if animTick then (flagFrame + 1) `mod` maxFrame else flagFrame
            lastAnim' = if animTick then timestamp inputs else lastAnim
            s'        = s { rcFlagFrame = flagFrame', rcLastAnim = lastAnim' }
            ViewMenu m = mkResearchCenterMenu gd cursor
            inner = case updateMenu inputs m of
                Just (Right upd) -> upd
                Just (Left m')   -> PureStep $ Refresh $ AnyGamePlayState
                                        (s' { rcCursor = cursorPosition (options m') }) True
                Nothing
                    | animTick || windowResized inputRes /= Nothing ->
                        PureStep $ Refresh $ AnyGamePlayState s' True
                    | otherwise -> PureStep UseCache
        in thinkWithPause gd inputRes inner

    draw (ResearchCenterState gd flagFrame _ cursor) gr _ =
        let xEnd = 620
            iY   = 250
            (image, iX, scale) = scalingRecenterImage gr xEnd iY 40 40 2.0 "institute"
            flagOffsetX = round (21 * scale)
            flagOffsetY = round (132 * scale)
            flagAnim    = APlace (iX + flagOffsetX) (iY + flagOffsetY) scale "flag_with_shadow" flagFrame 0 2 True
            funds       = gameDataFunds gd
            fundTxts    = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
            fundWords   = oneLine gr fundTxts 630 100 2
            words       = [ TextDisplay "Research" 50 10  7 White Nothing
                          , TextDisplay "Center"   200 140 7 White Nothing
                          ]
            v = View ((,0) <$> (words ++ fundWords)) [image] [flagAnim] [] Nothing
        in GameView v Nothing (Just $ mkResearchCenterMenu gd cursor)
