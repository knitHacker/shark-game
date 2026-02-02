{-# LANGUAGE Strict #-}

module GameData.Random
    ( getRandomPercent
    , getRandomPercentS
    , getRandomRange
    , getRandomRangeS
    , getRandomBool
    , getRandomBoolS
    , getRandomElem
    ) where

import System.Random.MWC as R
import Control.Monad.ST.Lazy (runST)
import qualified Data.List as L

import GameData.Types ( GameData(..) )

-- ============================================================================
-- Pure Random Functions (use deterministic Seed from GameData)
-- ============================================================================

-- | Get a random percentage (0-100) and updated GameData
getRandomPercent :: GameData -> (GameData, Int)
getRandomPercent gd = (gd { gameDataSeed = s' }, p)
  where
    s = gameDataSeed gd
    (s', p) = getRandomPercentS s

-- | Get a random Int in range [minVal, maxVal] and updated GameData
getRandomRange :: GameData -> Int -> Int -> (GameData, Int)
getRandomRange gd minVal maxVal = (gd { gameDataSeed = s' }, p)
  where
    s = gameDataSeed gd
    (s', p) = getRandomRangeS s minVal maxVal

-- | Get a random Bool and updated GameData
getRandomBool :: GameData -> (GameData, Bool)
getRandomBool gd = (gd { gameDataSeed = s' }, p)
  where
    s = gameDataSeed gd
    (s', p) = getRandomBoolS s

-- | Get a random element from a list and updated GameData
getRandomElem :: GameData -> [a] -> (GameData, a)
getRandomElem gd ls = runST $ do
    gen <- restore s
    p <- uniformR (0, len - 1) gen
    s' <- save gen
    return (gd { gameDataSeed = s' }, ls !! p)
  where
    s = gameDataSeed gd
    len = L.length ls

-- ============================================================================
-- Low-level pure random functions (work directly with Seed)
-- ============================================================================

-- | Get a random percentage (0-100) from a Seed, returning new Seed
getRandomPercentS :: Seed -> (Seed, Int)
getRandomPercentS s = runST $ do
    gen <- restore s
    p <- uniformR (0, 100) gen
    s' <- save gen
    return (s', p)

-- | Get a random Int in range [minVal, maxVal] from a Seed, returning new Seed
getRandomRangeS :: Seed -> Int -> Int -> (Seed, Int)
getRandomRangeS s minVal maxVal = runST $ do
    gen <- restore s
    p <- uniformR (minVal, maxVal) gen
    s' <- save gen
    return (s', p)

-- | Get a random Bool from a Seed, returning new Seed
getRandomBoolS :: Seed -> (Seed, Bool)
getRandomBoolS s = runST $ do
    gen <- restore s
    p <- uniformM gen
    s' <- save gen
    return (s', p)
