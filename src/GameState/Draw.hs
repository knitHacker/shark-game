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
import OutputHandles.Types
import InputState ( Direction(..) )

import Debug.Trace
import GHC.Real (fromIntegral)
import GameState.Collision.BoundBox


updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m) => m (Maybe ToRender)
updateWindow = do
    cfgs <- readConfigs
    gs <- readGameState
    case gs of
        (GameView _ m) -> return $ Just $ updateGameMenu 0 m
        (OverlayMenu top back) ->
            let backR = updateGameMenu 0 back
                bg = addRectangle renderEmpty 1 0 (DRectangle Yellow 20 20 150 200)
                frontR = updateGameMenu 2 $ overlayMenu top
            in return $ Just $ backR <> bg <> frontR
        _ -> return $ Just renderEmpty

updateGameMenu :: Int -> Menu -> ToRender
updateGameMenu d (Menu words _ opts) = r <> updateMenuOptions d opts
    where
        r = foldl (\rend td -> addText rend d 0 td) renderEmpty words

updateMenuOptions :: Int -> MenuOptions -> ToRender
updateMenuOptions _ (SelOneListOpts (OALOpts _ _ [] _)) = renderEmpty
updateMenuOptions _ (SelMultiListOpts (MSLOpts _ _ [] _)) = renderEmpty
updateMenuOptions d (SelOneListOpts (OALOpts x y ma (MenuCursor pos (CursorPointer tE)))) = r'
    where
        r = addTexture renderEmpty d 0 (DTexture t xPos' yPos' w h Nothing)
        r' = foldl (\rend td -> addText rend d 1 td) r $ updateMenuOptions' ma oX oY
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
        xPos = x - 20
        yPos = y + (20 * pos)
        xPos' = fromIntegral xPos
        yPos' = fromIntegral yPos
        bottom = yPos' + h
        oX = fromIntegral x
        oY = fromIntegral y
updateMenuOptions d (SelOneListOpts (OALOpts x y ma (MenuCursor pos (CursorRect c)))) = r'
    where
        r = addRectangle renderEmpty d 0 (DRectangle c xPos' yPos' w h)
        r' = foldl (\rend td -> addText rend d 1 td) r $ updateMenuOptions' ma oX oY
        xPos = x - 5
        yPos = y + (20 * pos) - 3
        xPos' = fromIntegral xPos
        yPos' = fromIntegral yPos
        opt = ma !! pos
        h = 20
        bottom = yPos' + h
        w = fromIntegral $ (T.length (menuOptionText opt) * 5) + 10
        oX = fromIntegral x
        oY = fromIntegral y

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
