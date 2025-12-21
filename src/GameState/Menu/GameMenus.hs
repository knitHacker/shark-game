{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GameState.Menu.GameMenus
    ( mainMenu
    , researchCenterMenu
    , introWelcome
    , introMission
    , introFunds
    , introEquipment
    , introResearch
    , introBoat
    , introEnd
    ) where


import qualified Data.Map.Strict as M
import qualified Data.Text as T


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
import Graphics.Types
import Graphics.Menu
import Graphics.TextUtil
import Graphics.ImageUtil
import Graphics.Animation

import InputState
import Util

import Debug.Trace

mainMenu :: Maybe GameData -> GameMenu
mainMenu gdM = GameMenu (textView words) (Menu optEntry Nothing)
    where
        optEntry = selOneOpts 400 550 3 2 menuOpts Nothing cursor 0
        cursor = CursorPointer "green_arrow"
        words = [ TextDisplay "Shark" 10 10 14 Gray Nothing
                , TextDisplay "Institute" 100 200 14 Gray Nothing
                , TextDisplay "Press ENTER to select" 200 460 3 White Nothing
                ]
        newGame = MenuAction "New Game" $ Just $ IntroWelcome Nothing
        continueGame cg = MenuAction "Continue" $ Just $ ResearchCenter cg
        exitOpt = MenuAction "Exit" $ Just $ GameExitState gdM
        menuOpts =
            case gdM of
                Nothing -> [newGame, exitOpt]
                Just cg -> [continueGame cg, newGame, exitOpt]


introWelcome :: GameData -> Graphics -> GameMenu
introWelcome gd gr = GameMenu (View ((,0) <$> words) imgs [] [] Nothing) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        welcomeText = "You and a group of shark enthusiasts / scientists want to learn more about sharks instead of \
                      \those marine mammals that seem to dominate the marine biology departments. \
                      \Together you decide to open a new research center dedicated entirely to"
        endText = "And you have been appointed the new director of the center!"
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100
        (wrappedWelcome, yEnd) = wrapTextMiddleX gr welcomeText 50 200 3 2 White
        words = [ fst $ textMiddleX gr "Welcome!" 20 12 White
                ] ++ wrappedWelcome ++
                [ fst $ textMiddleX gr "SHARKS!!!" (fromIntegral yEnd + 100) 4 White
                , fst $ textMiddleX gr endText 600 2 Green
                ]
        imgs = [IPlace 150 (yEnd + 20) 1.0 "no_dolphin" 1]
        opts = [ MenuAction "Next" $ Just $ IntroMission gd
               ]

introMission :: GameData -> Graphics -> GameMenu
introMission gd gr = GameMenu (textView words) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        howItWorksText = "As the director of the Shark Research Center, you will plan research trips \
                         \to find a variety of shark species in the wild, collect data about them, \
                         \and contribute to the scientific and the community's understanding of these \
                         \fascinating creatures by publishing your findings. By publishing new papers \
                         \the institute will gain recognition and funding to continue its important work. \
                         \You will need to manage your resources, plan research expeditions, and publish the data collected \
                         \to contribute to the understanding of sharks."
        (wrappedHowItWorks, yEnd) = wrapTextMiddleX gr howItWorksText 40 300 3 2 White
        words = TextDisplay "Mission" 50 20 9 White Nothing
              : TextDisplay "Statement" 150 150 9 White Nothing
              : wrappedHowItWorks
        opts = [ MenuAction "Next" $ Just $ IntroBoat gd
               ]
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100

introBoat :: GameData -> GameConfigs -> Graphics -> GameMenu
introBoat gd cfg gr = GameMenu (View ((,0) <$> words) [img] [] [] Nothing) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        startBoatKey = gameActiveBoat $ gameDataEquipment gd
        startBoat = boats (sharkCfgs cfg) M.! startBoatKey
        img = IPlace 500 400 2.0 (boatImage startBoat) 1
        boatText = "Luckily a retiring fisherman has decided to donate his skiff to your cause, \
                   \so you have a way to conduct your research trips. It might be old and small and smelly \
                   \but it floats (most of the time). Maybe in the future we can get another boat..."
        (wrappedBoat, yEnd) = wrapTextMiddleX gr boatText 75 320 3 2 White
        words = TextDisplay "A Kind" 40 20 8 White Nothing
              : TextDisplay "Neighbor" 100 130 9 White Nothing
              : wrappedBoat
        opts = [ MenuAction "Next" $ Just $ IntroEquipment gd ]
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100

introEquipment :: GameData -> GameConfigs -> Graphics -> GameMenu
introEquipment gd cfg gr = GameMenu (View words imgs [] rects Nothing) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        eqKeys = gameOwnedEquipment $ gameDataEquipment gd
        eqs = map (\k -> equipment (sharkCfgs cfg) M.! k) eqKeys
        eqCatch = head $ filter (\e -> equipInfoType e == Caught) eqs
        eqObs = head $ filter (\e -> equipInfoType e == Observed) eqs
        equipmentText = "With the initial funds you were also able to purchase some basic research equipment \
                        \to get your center up and running. It's not much, but it's a start. \
                        \Hopefully as you make progress in your research you can acquire more advanced gear."
        (wrappedEquip, yEnd) = wrapTextMiddleX gr equipmentText 70 500 3 2 White
        words = (TextDisplay "Equipment" 30 20 9 White Nothing, 0)
              : (TextDisplay "Catch" (fromIntegral rectStartX + 40) (fromIntegral rectStartY + 10) 3 Blue Nothing, 2)
              : (TextDisplay "Observe" (fromIntegral rectStartX + 340) (fromIntegral rectStartY + 10) 3 Blue Nothing, 2)
              : ((,0) <$> wrappedEquip)
        opts = [ MenuAction "Next" $ Just $ IntroResearch gd ]
        imgs = [ IPlace (rectStartX + 10) (rectStartY + 50) 4.0 (equipImage eqCatch) 2
               , IPlace (rectStartX + 345) (rectStartY + 50) 4.0 (equipImage eqObs) 2
               ]
        rectStartX = midStart gr 600
        rectStartY = 180
        rects = [ (LightGray, rectStartX, rectStartY, 600, 300, 1)]
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100

introResearch :: GameData -> Graphics -> GameMenu
introResearch gd gr = GameMenu (textView words) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        researchText = "The type of data you gather from each shark will depend on the type of equipment you used to \
                       \collect it. Using catch equipment will allow you to gather physical data such as size, weight, \
                       \and health indicators, while observation equipment will enable you to record behavioral data \
                       \such as feeding habits, social interactions, and movement patterns. As you discover more sharks \
                       \and publish more papers you will be inspired to start working on new research topics. Check back \
                       \at the research center to see what data you will need to collect to finish these new studies. \
                       \Once your next paper is published you are sure to be rolling in the grant money!"
        (wrappedResearch, yEnd) = wrapTextMiddleX gr researchText 50 300 4 2 White
        words = TextDisplay "Publishing" 30 20 9 White Nothing
              : TextDisplay "Your Findings" 80 150 8 White Nothing
              : wrappedResearch
        opts = [ MenuAction "Next" $ Just $ IntroFunds gd ]
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100

introFunds :: GameData -> Graphics -> GameMenu
introFunds gd gr = GameMenu (textView words) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        welcomeText = "All together we were able to raise initial funds to \
                      \support our research initiatives. These funds will help \
                      \cover the costs of equipment, travel, and other essential expenses. \
                      \Don't forget to check for new boats and equipment under the lab management menu!"
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100
        grantText = "Initial Funds: "
        startMoney = gameDataFunds gd
        gd' = gd { gameDataFunds = startMoney }
        (wrappedWelcome, yEnd) = wrapTextMiddleX gr welcomeText 50 250 3 2 White
        words = TextDisplay "Funds" 50 30 10 White Nothing
                : TextDisplay grantText 300 (fromIntegral yEnd + 100) 4 White Nothing
                : TextDisplay (showMoney startMoney) 500 600 3 Green Nothing
                : wrappedWelcome
        opts = [ MenuAction "Next" $ Just $ IntroEnd gd' ]

introEnd :: GameData -> Graphics -> GameMenu
introEnd gd gr = GameMenu (textView words) (Menu (selOneOpts optX optY 3 2 opts Nothing (CursorRect White) 0) Nothing)
    where
        endText = "With everything in place, you are now ready to begin your research expeditions. \
                  \Good luck, and may your studies contribute significantly to the understanding and conservation of sharks!"
        (wrappedEnd, yEnd) = wrapTextMiddleX gr endText 100 300 3 2 White
        words = TextDisplay "Good Luck!" 100 50 12 White Nothing : wrappedEnd
        optX = div (graphicsWindowWidth gr) 2
        optY = graphicsWindowHeight gr - 100
        opts = [ MenuAction "Begin Research" $ Just $ ResearchCenter gd ]

researchCenterMenu :: GameData -> InputState -> Graphics -> GameView
researchCenterMenu gd (InputState _ _ _ ts) gr = GameView v Nothing [animTo] $ Just m
    where
        v = View ((,0) <$> (words ++ fundWords)) [image] [flagAnim] [] Nothing
        m = Menu (selOneOpts 100 400 3 15 opts Nothing mc 0) Nothing
        funds = gameDataFunds gd
        mc = CursorRect White
        xEnd = 620
        iY = 250
        (image, iX, scale) = scalingRecenterImage gr xEnd iY 2.0 "institute"
        -- scale the offset of the start of the flag animation to put it in the right place
        flagOffsetX = round (21 * scale)
        flagOffsetY = round (132 * scale)
        flagAnim = APlace (iX + flagOffsetX) (iY + flagOffsetY) scale "flag_with_shadow" 0 0 2
        animTo = TimeoutData ts 170 $ TimeoutAnimation $ startTextAnim gr [flagAnim]
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
        words = [ TextDisplay "Research" 50 10 7 White Nothing
                , TextDisplay "Center" 200 140 7 White Nothing
                ]
        fundWords = oneLine gr fundTxts 630 100 2
        opts = [ MenuAction "Plan Research Trip" $ Just $ TripDestinationSelect gd
               , MenuAction "Review Data" $ Just $ DataReviewTop gd
               , MenuAction "Lab Management" $ Just $ LabManagement gd
               ]
