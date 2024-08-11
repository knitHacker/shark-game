{-# LANGUAGE OverloadedStrings #-}
module GameState.Draw
    ( updateWindow
    ) where


import Foreign.C.Types ( CInt )
import qualified SDL
import SDL.Vect ()
import SDL                    (($=))
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Text as T
import Data.Unique ( Unique )

import Configs
    ( ConfigsRead(..)
    , GameConfigs(..)
    )
import GameState ()
import GameState.Types
    ( GameStateRead(..)
    , MenuCursor(..)
    , Menu(..)
    , MenuAction(..)
    , MenuOptions(..)
    , GameState(..)
    , CursorType(..)
    )
import OutputHandles.Types
    ( Color(..)
    , Draw(..)
    , Draws
    , TextDisplay(..)
    , TextureEntry(..)
    , ToRender(..)
    , DrawTexture(..)
    , DrawRectangle(..)
    )
import OutputHandles.Draw ( mkRect )
import InputState ( Direction(..) )

import Debug.Trace
import GHC.Real (fromIntegral)
import GameState.Collision.BoundBox


updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m (Maybe ToRender)
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    case gs of
        GameMenu m -> return $ Just $ updateGameMenu m
        _ -> return $ Just $ ToRender M.empty [] []

updateGameMenu :: Menu -> ToRender
updateGameMenu (Menu _ words opts cur) = ToRender M.empty words [] <> updateMenuOptions cur opts

updateMenuOptions :: MenuCursor -> MenuOptions -> ToRender
updateMenuOptions _ (MenuOpts _ _ []) = ToRender M.empty [] []
updateMenuOptions (MenuCursor pos (CursorPointer tE)) mo = ToRender draws (updateMenuOptions' ma oX oY) []
    where
        draws = M.singleton (0, bottom, 0, xPos') (DrawTexture (DTexture t xPos' yPos' w h Nothing))
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
        xPos = optionXPos mo - 20
        yPos = optionYPos mo + (20 * pos)
        xPos' = fromIntegral xPos
        yPos' = fromIntegral yPos
        bottom = yPos' + h
        oX = fromIntegral $ optionXPos mo
        oY = fromIntegral $ optionYPos mo
        ma = menuOpts mo
updateMenuOptions (MenuCursor pos (CursorRect c)) mo = ToRender draws (updateMenuOptions' ma oX oY) []
    where
        draws = M.singleton (0, bottom, 0, xPos') (DrawRectangle (DRectangle c xPos' yPos' w h))
        xPos = optionXPos mo - 5
        yPos = optionYPos mo + (20 * pos) - 3
        xPos' = fromIntegral xPos
        yPos' = fromIntegral yPos
        opt = ma !! pos
        h = 20
        bottom = yPos' + h
        w = fromIntegral $ (T.length (menuOptionText opt) * 5) + 10
        oX = fromIntegral $ optionXPos mo
        oY = fromIntegral $ optionYPos mo
        ma = menuOpts mo


updateMenuOptions' :: [MenuAction] -> CInt -> CInt -> [TextDisplay]
updateMenuOptions' [] _ _ = []
updateMenuOptions' (h:tl) x y = dis : updateMenuOptions' tl x newY
    where
        newY = y + 20
        dis = TextDisplay str x y w 15 Blue
        (str, w) =
            let optText = menuOptionText h
                optLen = fromIntegral $ T.length optText
            in (optText, 5 * optLen)
