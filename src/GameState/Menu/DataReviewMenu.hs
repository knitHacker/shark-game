{-# LANGUAGE OverloadedStrings #-}

module GameState.Menu.DataReviewMenu
    ( topReviewMenu
    , sharkReviewMenu
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Util
import OutputHandles.Types
import Configs
import Shark.Types
import Shark.Review
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
sharkReviewMenu gd sharkEntry cfgs = mkMenu words' [] (selOneOpts 30 180 3 20 [returnOpt] mc) 0
    where
        sharkFinds = getFinds (sharkCfgs cfgs) gd sharkEntry
        mc = CursorRect White
        infoCnts = getInfoCounts sharkFinds
        returnOpt = MenuAction "Back" $ DataReviewTop gd
        sightingText = T.append "Shark interactions: " $ T.pack $ show $ length sharkFinds
        countTxts = M.mapWithKey (\i c -> T.concat ["Sharks ", i, " ", T.pack (show c)]) infoCnts
        words = [ TextDisplay (getData sharkEntry sharkName) 10 10 6 White
                , TextDisplay sightingText 20 40 4 White
                ]
        (_, words') = foldl (\(y, l) t -> (y + 20, l ++ [TextDisplay t 30 y 3 White])) (80, words) countTxts
