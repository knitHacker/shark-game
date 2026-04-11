{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.GameMenus
    ( initialMainMenuState
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Configs
import SaveData

import OutputHandles.Types ( Color(..), TextDisplay(TextDisplay) )

import GameState.Types
import GameState.Helpers
import Graphics.Types hiding (updateMenu)
import Graphics.Menu
import Graphics.TextUtil

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
    , mainMenuMenu     :: !(Menu AnyGamePlayState)
    }


mainMenuOpts :: Maybe GameData -> [MenuAction AnyGamePlayState]
mainMenuOpts gdM =
    let newGame         = MenuAction "New Game"  Nothing $ Just $ AnyGamePlayState (IntroWelcomeState Nothing) False
        continueGame gd = MenuAction "Continue"  Nothing $ Just $ AnyGamePlayState (ResearchCenterState gd)   False
        exitOpt         = MenuAction "Exit"       Nothing $ Just $ AnyGamePlayState GameExitState               False
    in case gdM of
        Nothing -> [newGame, exitOpt]
        Just gd -> [continueGame gd, newGame, exitOpt]


mkMainMenu :: Maybe GameData -> Graphics -> Int -> Menu AnyGamePlayState
mkMainMenu gdM gr cursor =
    let midY    = div (graphicsWindowHeight gr) 2
        instTxt = "Press Enter to select" :: T.Text
        xPos    = midTextStart gr instTxt 3
        md = selOneOpts (xPos + 100) (midY + 100) 3 2 (mainMenuOpts gdM) Nothing (CursorPointer "green_arrow") cursor
    in Menu md Nothing


instance GamePlayState MainMenuState where
    think _ _ = PureStep NoChange

    draw (MainMenuState gdM waves _ _ storedMenu) gr _ =
        let cursor   = cursorPosition (options storedMenu)
            midY     = div (graphicsWindowHeight gr) 2
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

    animate s@(MainMenuState _ waves frame lastAnim _) gr inputs
        | timestamp inputs - lastAnim >= waveAnimPeriod =
            let frame' = (frame + 1) `mod` waveFrameCount
            in s { mainMenuWaves    = updateWave gr frame' <$> zip [1..] waves
                 , mainMenuFrame    = frame'
                 , mainMenuLastAnim = timestamp inputs
                 }
        | otherwise = s

    handleInput s@(MainMenuState _ _ _ _ m) inputs =
        case updateMenu inputs m of
            Nothing           -> Left s
            Just (Left m')    -> Left s { mainMenuMenu = m' }
            Just (Right next) -> Right next


initialMainMenuState :: Maybe GameData -> AnyGamePlayState
initialMainMenuState gdM =
    let m = Menu (selOneOpts 0 0 3 2 (mainMenuOpts gdM) Nothing (CursorPointer "green_arrow") 0) Nothing
    in AnyGamePlayState (MainMenuState gdM initialWaves 0 0 m) True


-- ===========================================================================
-- Intro sequence — stubs, full draw/input in step 3
-- ===========================================================================

newtype IntroWelcomeState = IntroWelcomeState (Maybe GameData)

instance GamePlayState IntroWelcomeState where
    think (IntroWelcomeState Nothing)    _ =
        GenerateNewGame $ \gd -> Transition $ AnyGamePlayState (IntroWelcomeState (Just gd)) False
    think (IntroWelcomeState (Just _)) _ = PureStep NoChange
    draw  _ _ _                          = stubView

newtype IntroMissionState   = IntroMissionState   GameData
newtype IntroBoatState      = IntroBoatState      GameData
newtype IntroEquipmentState = IntroEquipmentState GameData
newtype IntroResearchState  = IntroResearchState  GameData
newtype IntroFundsState     = IntroFundsState     GameData
newtype IntroEndState       = IntroEndState       GameData

instance GamePlayState IntroMissionState   where { think _ _ = PureStep NoChange; draw _ _ _ = stubView }
instance GamePlayState IntroBoatState      where { think _ _ = PureStep NoChange; draw _ _ _ = stubView }
instance GamePlayState IntroEquipmentState where { think _ _ = PureStep NoChange; draw _ _ _ = stubView }
instance GamePlayState IntroResearchState  where { think _ _ = PureStep NoChange; draw _ _ _ = stubView }
instance GamePlayState IntroFundsState     where { think _ _ = PureStep NoChange; draw _ _ _ = stubView }
instance GamePlayState IntroEndState       where { think _ _ = PureStep NoChange; draw _ _ _ = stubView }


-- ===========================================================================
-- ResearchCenterState — stub, transitions to trip/lab/review added in step 3
-- ===========================================================================

newtype ResearchCenterState = ResearchCenterState GameData

instance GamePlayState ResearchCenterState where
    think _ _ = PureStep NoChange
    draw  _ _ _ = stubView


-- ===========================================================================

stubView :: GameView
stubView = GameView (View [] [] [] [] Nothing) Nothing Nothing
