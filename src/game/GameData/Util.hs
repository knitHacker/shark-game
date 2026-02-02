{-# LANGUAGE OverloadedStrings #-}

module GameData.Util
    ( sortSharks
    , addShark
    , getShark
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.List as L

import GameData.Types

-- | Sort sharks by species for quick lookup
-- Used when loading save data to rebuild the species index
sortSharks :: M.Map SharkIndex GameSharkData -> M.Map T.Text [SharkIndex]
sortSharks = M.foldlWithKey' (\m i sd -> M.insertWith (L.++) (gameSharkSpecies sd) [i] m) M.empty

-- | Add a shark encounter to the game data
-- Increments the shark index, inserts the shark, and updates the species index
addShark :: GameData -> GameSharkData -> GameData
addShark gd gsd = gd
    { gameDataSharkIndex = i + 1
    , gameDataSharks = M.insert i gsd (gameDataSharks gd)
    , gameDataFoundSharks = M.insertWith (L.++) (gameSharkSpecies gsd) [i] (gameDataFoundSharks gd)
    }
  where
    i = gameDataSharkIndex gd

-- | Get shark data by index
-- Throws an exception if the shark index doesn't exist
getShark :: GameData -> SharkIndex -> GameSharkData
getShark gd i = gameDataSharks gd M.! i
