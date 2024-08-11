{-# LANGUAGE OverloadedStrings #-}
module Env.Files
    ( getGameFullPath
    ) where

import Paths_shark_game

import System.Directory

lookupFuncs :: [FilePath -> IO FilePath]
lookupFuncs =
    [ return . id
    , getDataFileName
    ]


getGameFullPath :: FilePath -> IO FilePath
getGameFullPath file = getGameFullPath' file lookupFuncs

getGameFullPath' :: FilePath -> [(FilePath -> IO FilePath)] -> IO FilePath
getGameFullPath' fp [] = error $ "Failed to find file " ++ fp
getGameFullPath' fp (hf:tl) = do
    path <- hf fp
    exists <- doesFileExist path
    if exists
        then return path
        else getGameFullPath' fp tl
