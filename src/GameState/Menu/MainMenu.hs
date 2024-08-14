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


researchCenterOpt :: GameData -> OptAction
researchCenterOpt gd _ _ o = GameView (Just (pauseMenu rcM gd)) rcM
    where
        rcM = initResearchCenterMenu o gd

initMainMenu :: GameConfigs -> OutputHandles -> IO GameState
initMainMenu cfgs outs = do
    nGame <- startNewGame
    gdM <- case lastSaveM (stateCfgs cfgs) of
                Nothing -> return Nothing
                Just sf -> do
                    gd <- loadFromFile sf
                    return $ Just gd
    return $ GameView Nothing $ Menu words [] $ optEntry nGame gdM

    where
        optEntry nGame gdM = selOneOpts 80 180 (menuOpts nGame gdM) cursor
        arrowEntry = CursorPointer $ textures outs M.! "green_arrow"
        cursor = MenuCursor 0 arrowEntry
        words = [ TextDisplay "Shark Research" 10 10 200 100 Gray
                , TextDisplay "Press ENTER to select" 75 150 100 20 White
                ]
        newGame ng = MenuAction "New Game" (\_ _ o -> GameView Nothing  (introPage o ng))
        continueGame cg = MenuAction "Continue" (\_ _ o -> GameView Nothing (introPage o cg))
        exitOpt = MenuAction "Exit" exitMenuAction
        menuOpts ng cgM =
            case cgM of
                Nothing -> [newGame ng, exitOpt]
                Just cg -> [continueGame cg, newGame ng, exitOpt]


introPage :: OutputHandles -> GameData -> Menu
introPage outs gd = Menu words [] $ selOneOpts 80 220 opts $ MenuCursor 0 $ CursorRect Gray
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
        opts = [ MenuAction "Start Research" (researchCenterOpt gd')
               ]



initResearchCenterMenu :: OutputHandles -> GameData -> Menu
initResearchCenterMenu outs gd = Menu words [] $ selOneOpts 15 140 opts mc
    where
        funds = gameDataFunds gd
        mc = MenuCursor 0 cursorT
        cursorT = CursorRect Gray
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        words = [ TextDisplay "Research Center" 10 10 200 100 White
                , TextDisplay fundTxt 75 110 80 12 Green
                ]
        mm c = mapMenu c gd
        opts = [ MenuAction "Plan Research Trip" (\c _ _ -> GameView (Just (pauseMenu (mm c) gd)) (mm c))
               , MenuAction "Review Data" undefined
               , MenuAction "Lab Management" undefined
               ]

mapMenu :: GameConfigs -> GameData -> Menu
mapMenu cfgs gd = Menu words [] $ selOneOpts 15 140 opts' mc
    where
        locs = (\(loc, lCfg) -> (loc, showText lCfg)) <$> (M.assocs $ siteLocations $ sharkCfgs cfgs)
        mc = MenuCursor 0 $ CursorRect Gray
        words = [ TextDisplay "Select Trip Destination" 10 10 200 100 White
               ]
        opts = (\(loc, txt) -> MenuAction txt undefined) <$> locs
        rtOpt = MenuAction "Return to Lab" (researchCenterOpt gd)
        opts' = opts ++ [rtOpt]

-- equipmentPickMenu
