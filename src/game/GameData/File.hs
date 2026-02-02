module GameData.File
    ( makeSaveFilePath
    , getSaveFilePath
    , maxRand
    ) where

import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import qualified Data.List as L

import Env.Files (getLocalGamePath)

-- | Maximum random number for save file names
maxRand :: Int
maxRand = 999999999

-- | Number of digits for zero-padding save file names
maxRandDigits :: Int
maxRandDigits = length (show maxRand)

-- | Generate a save file name from a random number
makeFileName :: Int -> String
makeFileName n = "shark-" L.++ printf ("%0" L.++ show maxRandDigits L.++ "d") n L.++ ".save"

-- | Create a full save file path from a random number
-- Creates the saves directory if it doesn't exist
makeSaveFilePath :: Int -> IO FilePath
makeSaveFilePath randomNum = do
    let fileName = makeFileName randomNum
        relativePath = "data" </> "saves" </> fileName
    fullPath <- getLocalGamePath relativePath
    createDirectoryIfMissing True (takeDirectory fullPath)
    return fullPath

-- | Get the full path to a save file by name
-- Creates the saves directory if it doesn't exist
getSaveFilePath :: String -> IO FilePath
getSaveFilePath fileName = do
    let relativePath = "data" </> "saves" </> fileName
    fullPath <- getLocalGamePath relativePath
    createDirectoryIfMissing True (takeDirectory fullPath)
    return fullPath
