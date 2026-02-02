module Main where

import System.Environment (getArgs)

import Configs
import SaveData
import Generate

main :: IO ()
main = do
    args <- getArgs
    let (scenario, outputPath) = case args of
            (s:p:_) -> (s, p)
            (s:_)   -> (s, "test-save.save")
            []      -> ("default", "test-save.save")

    (_, cfgs) <- initConfigs
   -- gd <- generateScenario cfgs scenario
   -- let gd' = gd { gameDataSaveFile = outputPath }
   -- saveToFile gd'
    putStrLn $ "Generated: " ++ outputPath


--generateScenario :: GameConfigs -> String -> IO GameData
--generateScenario cfgs scenario = do
--    base <- defaultGameData cfgs
--    case scenario of
--        "rich"       -> return $ withFunds base 1000000
--        "manysharks" -> return $ withSharks base (concat $ replicate 10 (sampleSharks cfgs))
--        "endgame"    -> return $ withFunds (withSharks base (sampleSharks cfgs)) 500000
--        _            -> return base

