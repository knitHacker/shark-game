{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.GameMenus
    ( mainMenu
    , researchCenterMenu
    , introPage
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
import Util

import Debug.Trace

mainMenu :: Maybe GameData -> GameMenu
mainMenu gdM = GameMenu (textView words) (Menu optEntry Nothing)
    where
        optEntry = selOneOpts 400 550 3 2 menuOpts cursor 0
        cursor = CursorPointer "green_arrow"
        words = [ TextDisplay "Shark" 10 10 14 Gray Nothing
                , TextDisplay "Research" 100 200 14 Gray Nothing
                , TextDisplay "Press ENTER to select" 200 460 3 White Nothing
                ]
        newGame = MenuAction "New Game" $ Just IntroPage
        continueGame cg = MenuAction "Continue" $ Just $ ResearchCenter cg
        exitOpt = MenuAction "Exit" $ Just $ GameExitState gdM
        menuOpts =
            case gdM of
                Nothing -> [newGame, exitOpt]
                Just cg -> [continueGame cg, newGame, exitOpt]


introPage :: GameData -> GameMenu
introPage gd = GameMenu (textView words) (Menu (selOneOpts 450 700 3 2 opts (CursorRect White) 0) Nothing)
    where
        welcomeText1 = "You are a new researcher at the Shark Research Institute."
        welcomeText2 = "Your new position has inspired a national research committee"
        welcomeText3 = "to issue an initial grant to start your work."
        grantText = "Grant Amount: "
        startMoney = gameDataFunds gd
        gd' = gd { gameDataFunds = startMoney }
        words = [ TextDisplay "Welcome!" 100 50 14 White Nothing
                , TextDisplay welcomeText1 50 300 2 White Nothing
                , TextDisplay welcomeText2 50 350 2 White Nothing
                , TextDisplay welcomeText3 50 400 2 White Nothing
                , TextDisplay grantText 400 500 4 White Nothing
                , TextDisplay (showMoney startMoney) 500 600 3 Green Nothing
                ]
        opts = [ MenuAction "Start Research" $ Just $ ResearchCenter gd'
               ]



researchCenterMenu :: GameData -> Graphics -> GameMenu
researchCenterMenu gd gr = GameMenu (textView (words ++ fundWords)) (Menu (selOneOpts 300 500 4 15 opts mc 0) Nothing)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (showMoney funds, Green, 3)]
        words = [ TextDisplay "Research" 50 10 10 White Nothing
                , TextDisplay "Center" 200 160 10 White Nothing
                ]
        fundWords = oneLine gr fundTxts 200 350 2
        opts = [ MenuAction "Plan Research Trip" $ Just $ TripDestinationSelect gd
               , MenuAction "Review Data" $ Just $ DataReviewTop gd
               , MenuAction "Lab Management" $ Just $ LabManagement gd
               ]
