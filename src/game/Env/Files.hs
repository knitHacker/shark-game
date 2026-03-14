{-# LANGUAGE OverloadedStrings #-}
module Env.Files
    ( getGameFullPath
    , getLocalGamePath
    , getGameDirectory
    , configFile
    , texturesFile
    , stateFile
    , sharkFile
    , researchFile
    , mechanicsFile
    , fontFile
    ) where

import Paths_shark_game

import System.Directory
import System.FilePath ((</>))

configFile :: FilePath
configFile = "data/configs/game.yaml"

texturesFile :: FilePath
texturesFile = "data/configs/textures.yaml"

stateFile :: FilePath
stateFile = "data/configs/state.yaml"

sharkFile :: FilePath
sharkFile = "data/configs/shark_game.yaml"

researchFile :: FilePath
researchFile = "data/configs/research.yaml"

mechanicsFile :: FilePath
mechanicsFile = "data/configs/mechanics.yaml"

fontFile :: FilePath
fontFile = "assets/fonts/saxmono.ttf"

lookupFuncs :: [FilePath -> IO FilePath]
lookupFuncs =
    [ return . id
    , getDataFileName
    ]

getGameDirectory :: FilePath -> IO FilePath
getGameDirectory dir = getGameFullPath' dir False lookupFuncs


getGameFullPath :: FilePath -> IO FilePath
getGameFullPath file = getGameFullPath' file True lookupFuncs


getGameFullPath' :: FilePath -> Bool -> [FilePath -> IO FilePath] -> IO FilePath
getGameFullPath' fp True [] = error $ "File not found: " ++ fp
getGameFullPath' fp False [] = do
    createDirectoryIfMissing True fp
    return fp
getGameFullPath' fp isFile (hf:tl) = do
    path <- hf fp
    exists <- if isFile then doesFileExist path else doesDirectoryExist path
    if exists
        then return path
        else getGameFullPath' fp isFile tl


getLocalGamePath :: FilePath -> IO FilePath
getLocalGamePath fp = do
    xdgDir <- getXdgDirectory XdgData "shark-game"
    return (xdgDir </> fp)
