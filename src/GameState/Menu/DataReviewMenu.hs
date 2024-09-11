{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.DataReviewMenu
    ( topReviewMenu
    , sharkReviewMenu
    ) where

import qualified Data.Map.Strict as M

import Util
import OutputHandles.Types
import Configs
import Shark.Types
import GameState.Types
import SaveData

topReviewMenu :: GameData -> GameConfigs -> Menu
topReviewMenu gd cfgs = mkMenu words [] (selOneOpts 20 90 3 20 (opts ++ otherOpts) mc) 0
    where
        sharksInfo = sharks $ sharkCfgs cfgs
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 10 10 10 White
                , TextDisplay "Discovered Sharks" 15 60 5 Green
                ]
        opts = (\s -> MenuAction (sharkName (sharksInfo M.! s)) (SharkReview gd (getEntry sharksInfo s))) <$> (M.keys (gameDataFoundSharks gd))
        otherOpts = [ MenuAction "Return to Research Center" $ ResearchCenter gd ]

sharkReviewMenu :: GameData -> DataEntry SharkInfo -> GameConfigs -> Menu
sharkReviewMenu gd sharkEntry cfgs = mkMenu words [] (selOneOpts 30 120 3 20 [returnOpt] mc) 0
    where
        mc = CursorRect White
        returnOpt = MenuAction "Back" $ DataReviewTop gd
        words = [ TextDisplay (getData sharkEntry sharkName) 10 10 8 White
                ]
