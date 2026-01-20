-- test/Spec.hs
module Main where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map.Strict as M
import System.Random (mkStdGen)

import Configs
import SaveData
import SaveData.Generate
import GameState.Types
import Shark.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Shark operations" $ do
        it "selling a shark removes it and adds funds" $ property $ \seed ->
            ioProperty $ do
                (_, cfgs) <- initConfigs
                gd <- generateRandomSharks cfgs seed 10
                let shark = head $ M.elems (gameDataSharks gd)
                    gd' = sellShark (sharkId shark) gd
                return $
                    M.notMember (sharkId shark) (gameDataSharks gd') &&
                    gameDataFunds gd' > gameDataFunds gd

        it "releasing a shark removes it without adding funds" $ property $ \seed ->
            ioProperty $ do
                (_, cfgs) <- initConfigs
                gd <- generateRandomSharks cfgs seed 5
                let shark = head $ M.elems (gameDataSharks gd)
                    initialFunds = gameDataFunds gd
                    gd' = releaseShark (sharkId shark) gd
                return $
                    M.notMember (sharkId shark) (gameDataSharks gd') &&
                    gameDataFunds gd' == initialFunds

        it "shark count stays consistent through catches" $ property $ \(seed, n) ->
            n > 0 && n < 100 ==> ioProperty $ do
                (_, cfgs) <- initConfigs
                gd <- generateRandomSharks cfgs seed n
                return $ M.size (gameDataSharks gd) == n

-- src/SaveData/Generate.hs additions
generateRandomSharks :: GameConfigs -> Int -> Int -> IO GameData
generateRandomSharks cfgs seed count = do
    base <- defaultGameData cfgs
    let gen = mkStdGen seed
        sharks = generateSharksWithSeed gen cfgs count
    return $ withSharks base sharks

generateSharksWithSeed :: StdGen -> GameConfigs -> Int -> [Shark]
generateSharksWithSeed gen cfgs n = take n $ unfoldr (Just . randomShark cfgs) gen

randomShark :: GameConfigs -> StdGen -> (Shark, StdGen)
randomShark cfgs gen = -- your shark generation logic using the config's species list, etc.

