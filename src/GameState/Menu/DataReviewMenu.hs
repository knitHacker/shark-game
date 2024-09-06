module GameState.Menu.DataReviewMenu (
    ) where

import qualified Data.Map.Strict as M

import Configs
import Shark.Types

import GameState.Types

topReviewMenu :: GameData -> GameConfigs -> Menu
topReviewMenu gd cfgs = mkMenu words [] (selOneOpts 20 60 4 20 opts mc) 0
    where
        sharksInfo = sharks $ sharkCfgs cfgs
        mc = CursorRect White
        words = [ TextDisplay "Data Review" 10 10 12 White ]
        opts = (\s -> MenuAction (sharksInfo M.! s) undefined) <$> (saveFoundSharks gd)
