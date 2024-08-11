{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.MainMenu
    ( initMainMenu
    , initResearchCenterMenu
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

import GameState.Menu
import GameState.Types
    ( MenuCursor(MenuCursor)
    , Menu(Menu)
    , MenuAction(..)
    , MenuOptions(..)
    , GameState(..)
    , CursorType(..)
    )

initMainMenu :: GameConfigs -> OutputHandles -> IO GameState
initMainMenu cfgs outs = do
    gdM <- return Nothing -- todo: try to load old game
    nGame <- startNewGame
    return $ StartMenu $ Menu False words (MenuOpts 80 180 (menuOpts nGame gdM)) cursor

    where
        arrowEntry = CursorPointer $ textures outs M.! "green_arrow"
        cursor = MenuCursor 0 arrowEntry
        words = [ TextDisplay "Shark Research" 10 10 200 100 Gray
                , TextDisplay "Press ENTER to select" 75 150 100 20 White
                ]
        newGame ng = MenuAction "New Game" (\_ _ o -> GameMenu (introPage o ng) ng)
        continueGame cg = MenuAction "Continue" (\_ _ o -> GameMenu (introPage o cg) cg)
        exitOpt = MenuAction "Exit" exitMenuAction
        menuOpts ng cgM =
            case cgM of
                Nothing -> [newGame ng, exitOpt]
                Just cg -> [continueGame cg, newGame ng, exitOpt]


introPage :: OutputHandles -> GameData -> Menu
introPage outs gd = Menu True words (MenuOpts 80 220 opts) (MenuCursor 0 (CursorRect Gray))
    where
        welcomeText1 = "You are a new researcher at the Shark Research Institute."
        welcomeText2 = "Your new position has inspired a national research committee"
        welcomeText3 = "to issue an initial grant to start your work."
        grantText = "Grant Amount: "
        startMoney = "$2,000"
        words = [ TextDisplay "Welcome!" 10 10 175 80 White
                , TextDisplay welcomeText1 8 100 ((fromIntegral (T.length welcomeText1))*3) 10 White
                , TextDisplay welcomeText2 8 120 ((fromIntegral (T.length welcomeText2))*3) 10 White
                , TextDisplay welcomeText3 8 140 ((fromIntegral (T.length welcomeText3))*3) 10 White
                , TextDisplay grantText 75 170 80 12 White
                , TextDisplay startMoney 80 190 60 12 Green
                ]
        rCM _ _ o = GameMenu (initResearchCenterMenu o gd) gd
        opts = [ MenuAction "Start Research" rCM
               ]



initResearchCenterMenu :: OutputHandles -> GameData -> Menu
initResearchCenterMenu outs _ = Menu True words (MenuOpts 15 110 opts) (MenuCursor 0 cursorT)
    where
        --arrowEntry = CursorPointer $ textures outs M.! "green_arrow"
        cursorT = CursorRect Gray
        words = [ TextDisplay "Research Center" 10 10 200 100 White
                ]
        opts = [ MenuAction "Plan Research Trip" undefined
               , MenuAction "Review Data" undefined
               , MenuAction "Lab Management" undefined
               ]
