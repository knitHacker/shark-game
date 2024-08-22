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
mainMenu gdM cfgs outs = mkMenu words [] optEntry 0
    where
        optEntry = selOneOpts 80 180 3 2 menuOpts cursor
        cursor = CursorPointer $ textures outs M.! "green_arrow"
        words = [ TextDisplay "Shark" 10 10 14 Gray
                , TextDisplay "Research" 20 60 14 Gray
                , TextDisplay "Press ENTER to select" 50 150 3 White
                ]
        newGame = MenuAction "New Game" IntroPage
        continueGame cg = MenuAction "Continue" $ ResearchCenter cg
        exitOpt = MenuAction "Exit" $ GameExitState gdM
        menuOpts =
            case gdM of
                Nothing -> [newGame, exitOpt]
                Just cg -> [continueGame cg, newGame, exitOpt]


introPage :: GameData -> Menu
introPage gd = mkMenu words [] (selOneOpts 80 220 3 2 opts (CursorRect Gray)) 0
    where
        welcomeText1 = "You are a new researcher at the Shark Research Institute."
        welcomeText2 = "Your new position has inspired a national research committee"
        welcomeText3 = "to issue an initial grant to start your work."
        grantText = "Grant Amount: "
        startMoney = "$2,000"
        gd' = gd { gameDataFunds = 2000 }
        words = [ TextDisplay "Welcome!" 10 10 14 White
                , TextDisplay welcomeText1 5 100 2 White
                , TextDisplay welcomeText2 5 120 2 White
                , TextDisplay welcomeText3 5 140 2 White
                , TextDisplay grantText 75 170 4 White
                , TextDisplay startMoney 100 190 3 Green
                ]
        opts = [ MenuAction "Start Research" $ ResearchCenter gd'
               ]



researchCenterMenu :: GameData -> OutputHandles -> Menu
researchCenterMenu gd outs = mkMenu words [] (selOneOpts 15 140 4 20 opts mc) 0
    where
        funds = gameDataFunds gd
        mc = CursorRect Gray
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        words = [ TextDisplay "Research" 10 10 12 White
                , TextDisplay "Center" 40 60 12 White
                , TextDisplay fundTxt 55 110 4 Green
                ]
        opts = [ MenuAction "Plan Research Trip" $ TripDestinationSelect gd
               , MenuAction "Review Data" ComingSoon
               , MenuAction "Lab Management" ComingSoon
               ]

