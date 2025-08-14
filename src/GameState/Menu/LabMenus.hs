{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.LabMenus
    ( labTopMenu
    ) where

import GameState.Types
import OutputHandles.Types
import OutputHandles.Util

import SaveData
import GameState.Types
import Shark.Trip

import qualified Data.Text as T

labTopMenu :: GameData -> OutputHandles -> Menu
labTopMenu gd outs = mkMenu (words ++ fundWords) [] (selOneOpts 15 160 4 15 opts mc 0)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (T.append "$" (T.pack (show funds)), Green, 3)]
        dateTxt2 = monthToText (gameDataMonth gd)
        words = [ TextDisplay "Research" 10 10 10 White
                , TextDisplay "Center" 40 45 10 White
                , TextDisplay dateTxt2 55 120 3 Green
                ]
        fundWords = oneLine outs fundTxts 35 90 2
        opts = [ MenuAction "Fundraising" False ComingSoon
               , MenuAction "Fleet Management" False ComingSoon
               , MenuAction "Equipment Management" False ComingSoon
               ]

-- To make sure the user doesn't get stuck with no money they can "host" a fundraiser
-- This will give a small amount of money but also will add a new name for a donor to
-- the research center's name
fundraiserTopMenu :: GameData -> OutputHandles -> Menu
fundraiserTopMenu gd outs = mkMenu (words ++ fundWords) [] (selOneOpts 15 160 4 15 opts mc 0)
    where
        funds = gameDataFunds gd
        mc = CursorRect White
        fundTxts = [("Current Funds: ", White, 3), (T.append "$" (T.pack (show funds)), Green, 3)]
        dateTxt = "Center research run time:"
        words = [ TextDisplay "Research" 10 10 10 White
                , TextDisplay "Center" 40 45 10 White
                , TextDisplay dateTxt 35 105 3 White
                ]
        fundWords = oneLine outs fundTxts 35 90 2
        opts = [ MenuAction "Host Fundraiser" False ComingSoon
               , MenuAction "Fleet Management" False ComingSoon
               , MenuAction "Equipment Management" False ComingSoon
               ]
