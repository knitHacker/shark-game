{-# LANGUAGE OverloadedStrings #-}
module Env.Files
    ( getGameFullPath
    , getGameDirectory
    ) where

import Paths_shark_game

import System.Directory

lookupFuncs :: [FilePath -> IO FilePath]
lookupFuncs =
    [ return . id
    , getDataFileName
    ]

getGameDirectory :: FilePath -> IO FilePath
getGameDirectory dir = getGameFullPath' dir False lookupFuncs

getGameFullPath :: FilePath -> IO FilePath
getGameFullPath file = getGameFullPath' file True lookupFuncs

getGameFullPath' :: FilePath -> Bool -> [(FilePath -> IO FilePath)] -> IO FilePath
getGameFullPath' fp False [] = do
    createDirectoryIfMissing True fp
    return fp
getGameFullPath' fp _ [] = error $ "Failed to find file " ++ fp
getGameFullPath' fp isFile (hf:tl) = do
    path <- hf fp
    exists <- if isFile then doesFileExist path else doesDirectoryExist path
    if exists
        then return path
        else getGameFullPath' fp isFile tl
