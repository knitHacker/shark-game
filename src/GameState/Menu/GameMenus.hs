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

import OutputHandles.Types
    ( Color(..)
    , OutputHandles(textures)
    , TextDisplay(TextDisplay)
    )

import GameState.Types

import Debug.Trace

mainMenu :: Maybe GameData -> GameConfigs -> OutputHandles -> Menu
mainMenu gdM cfgs outs = Menu words [] optEntry 0
    where
        optEntry = selOneOpts 80 180 menuOpts cursor
        cursor = CursorPointer $ textures outs M.! "green_arrow"
        words = [ TextDisplay "Shark Research" 10 10 200 100 Gray
                , TextDisplay "Press ENTER to select" 75 150 100 20 White
                ]
        newGame = MenuAction "New Game" IntroPage
        continueGame cg = MenuAction "Continue" $ ResearchCenter cg
        exitOpt = MenuAction "Exit" $ GameExitState gdM
        menuOpts =
            case gdM of
                Nothing -> [newGame, exitOpt]
                Just cg -> [continueGame cg, newGame, exitOpt]


introPage :: GameData -> Menu
introPage gd = Menu words [] (selOneOpts 80 220 opts (CursorRect Gray)) 0
    where
        welcomeText1 = "You are a new researcher at the Shark Research Institute."
        welcomeText2 = "Your new position has inspired a national research committee"
        welcomeText3 = "to issue an initial grant to start your work."
        grantText = "Grant Amount: "
        startMoney = "$2,000"
        gd' = gd { gameDataFunds = 2000 }
        words = [ TextDisplay "Welcome!" 10 10 175 80 White
                , TextDisplay welcomeText1 8 100 ((fromIntegral (T.length welcomeText1))*3) 10 White
                , TextDisplay welcomeText2 8 120 ((fromIntegral (T.length welcomeText2))*3) 10 White
                , TextDisplay welcomeText3 8 140 ((fromIntegral (T.length welcomeText3))*3) 10 White
                , TextDisplay grantText 75 170 80 12 White
                , TextDisplay startMoney 80 190 60 12 Green
                ]
        opts = [ MenuAction "Start Research" $ ResearchCenter gd'
               ]



researchCenterMenu :: GameData -> OutputHandles -> Menu
researchCenterMenu gd outs = Menu words [] (selOneOpts 15 140 opts mc) 0
    where
        funds = gameDataFunds gd
        mc = CursorRect Gray
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        words = [ TextDisplay "Research Center" 10 10 200 100 White
                , TextDisplay fundTxt 75 110 80 12 Green
                ]
        opts = [ MenuAction "Plan Research Trip" $ TripDestinationSelect gd
               , MenuAction "Review Data" ComingSoon
               , MenuAction "Lab Management" ComingSoon
               ]

