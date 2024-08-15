{-# LANGUAGE OverloadedStrings #-}
module GameState.Menu.GameMenus
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

import Debug.Trace


initMainMenu :: GameConfigs -> OutputHandles -> IO GameState
initMainMenu cfgs outs = do
    nGame <- startNewGame
    gdM <- case lastSaveM (stateCfgs cfgs) of
                Nothing -> return Nothing
                Just sf -> do
                    gd <- loadFromFile sf
                    return $ Just gd
    return $ mainMenu gdM nGame cfgs outs

mainMenu :: Maybe GameData -> GameData -> GameConfigs -> OutputHandles -> GameState
mainMenu gdM ng cfgs outs = GameView Nothing $ Menu words [] optEntry 0
    where
        optEntry = selOneOpts 80 180 menuOpts cursor
        cursor = CursorPointer $ textures outs M.! "green_arrow"
        words = [ TextDisplay "Shark Research" 10 10 200 100 Gray
                , TextDisplay "Press ENTER to select" 75 150 100 20 White
                ]
        newGame = MenuAction "New Game" (\_ _ o -> GameView Nothing  (introPage o ng))
        continueGame cg = MenuAction "Continue" $ researchCenterOpt cg
        exitOpt = MenuAction "Exit" exitMenuAction
        menuOpts =
            case gdM of
                Nothing -> [newGame, exitOpt]
                Just cg -> [continueGame cg, newGame, exitOpt]


researchCenterOpt :: GameData -> OptAction
researchCenterOpt gd _ _ o = GameView (Just (pauseMenu rcM gd)) rcM
    where
        rcM = initResearchCenterMenu o gd

introPage :: OutputHandles -> GameData -> Menu
introPage outs gd = Menu words [] (selOneOpts 80 220 opts (CursorRect Gray)) 0
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
initResearchCenterMenu outs gd = Menu words [] (selOneOpts 15 140 opts mc) 0
    where
        funds = gameDataFunds gd
        mc = CursorRect Gray
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
mapMenu cfgs gd = Menu words [] (selOneOpts 15 140 opts' mc) 0
    where
        locs = (\(loc, lCfg) -> (lCfg, showText lCfg)) <$> (M.assocs $ siteLocations $ sharkCfgs cfgs)
        mc = CursorRect Gray
        words = [ TextDisplay "Select Trip Destination" 10 10 220 80 White
                ]
        eMenu loc c = equipmentPickMenu loc [] 0 c gd
        opts = (\(loc, txt) -> MenuAction txt (\c _ _ -> GameView (Just (pauseMenu (eMenu loc c) gd)) (eMenu loc c))) <$> locs
        rtOpt = MenuAction "Return to Lab" (researchCenterOpt gd)
        opts' = opts ++ [rtOpt]

equipmentPickMenu :: GameLocation -> [T.Text] -> Int -> GameConfigs -> GameData -> Menu
equipmentPickMenu loc chsn pos cfgs gd = Menu words [] (selMultOpts 15 120 opts' update act (Just back)) pos
    where
        eq = equipment $ sharkCfgs cfgs
        rEs = requiredEquipment loc
        aEs = allowedEquipment loc
        lupE et =
            let eqEntry = eq M.! et
            in (et, T.append (T.append (text eqEntry) " - $") (T.pack (show (price eqEntry))))
        rEs' = lupE <$> rEs
        aEs' = lupE <$> aEs
        cost = foldl (\s opt -> if selectSelected opt then s + price (eq M.! (selectKey opt)) else s) 0 opts'
        costTxt = T.append "Trip Current Cost: $" (T.pack (show cost))
        words = [ TextDisplay "Select Trip Equipment" 10 10 220 80 White
                , TextDisplay costTxt 10 90 100 15 White
                ]
        rOpts = (\(k, t) -> SelectOption t k True False) <$> rEs'
        wasChsn t = elem t chsn
        aOpts = (\(k, t) -> SelectOption t k (wasChsn k) True) <$> aEs'
        opts' = rOpts ++ aOpts
        eMenu chsn p c = equipmentPickMenu loc chsn p c gd
        rMenu chsn c = reviewTripMenu loc chsn c gd
        update chsn p c _ _ = GameView (Just (pauseMenu (eMenu chsn p c) gd)) (eMenu chsn p c)
        act keys c _ _ = GameView (Just (pauseMenu (rMenu keys c) gd)) (rMenu keys c)
        mMenu c = mapMenu c gd
        back c _ _ = GameView (Just (pauseMenu (mMenu c) gd)) (mMenu c)


reviewTripMenu :: GameLocation -> [T.Text] -> GameConfigs -> GameData -> Menu
reviewTripMenu loc eqs cfgs gd = Menu words [] (selOneOpts 80 180 opts (CursorRect Gray)) 0
    where
        trip = tripInfo (sharkCfgs cfgs) loc eqs
        funds = gameDataFunds gd
        tc = tripCost trip
        enoughFunds = funds >= tc
        fundTxt = T.append "Current Funds: $" (T.pack (show funds))
        tripTxt = T.append "Trip Cost: $" (T.pack (show tc))
        afterTxt = T.append "After Trip: $" (T.pack (show (funds - tc)))
        tripLenTxt = T.append (T.append "Trip Length: " (T.pack (show (tripLength trip)))) " month(s)"
        words = [ TextDisplay "Review Trip Details" 10 10 220 80 White
                , TextDisplay fundTxt 25 95 85 12 Green
                , TextDisplay tripTxt 25 115 80 12 Red
                , TextDisplay afterTxt 25 135 80 12 (if enoughFunds then Green else Red)
                , TextDisplay tripLenTxt 120 95 80 12 White
                ]
        gd' = gd { gameDataFunds = funds - tc, gameDataMonth = gameDataMonth gd + tripLength trip }
        bMenu c = equipmentPickMenu loc eqs 0 c gd
        rMenu c = reviewTripMenu loc eqs c gd
        rGV c _ _ = GameView (Just (pauseMenu (rMenu c) gd)) (rMenu c)
        opts = [ MenuAction "Start Trip" (if enoughFunds then researchCenterOpt gd' else rGV)
               , MenuAction "Back to equipment" (\c _ _ -> GameView (Just (pauseMenu (bMenu c) gd)) (bMenu c))
               , MenuAction "Abort Trip" (researchCenterOpt gd)
               ]

