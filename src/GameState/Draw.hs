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
updateMenuOptions d (SelOneListOpts oalOpt) = updateSelOneListOptions d oalOpt


updateSelOneListOptions :: Int -> OneActionListOptions -> ToRender
updateSelOneListOptions _ (OALOpts _ _ [] _) = renderEmpty
updateSelOneListOptions d (OALOpts x y ma (MenuCursor pos curs)) = r'
    where
        r = updateListCursor d x yPos' cL curs
        r' = foldl (\rend td -> addText rend d 1 td) r $ updateMenuListOptions ma oX oY
        yPos = y + (20 * pos)
        yPos' = fromIntegral yPos
        oX = fromIntegral x
        oY = fromIntegral y
        opt = ma !! pos
        cL = T.length $ menuOptionText opt

updateListCursor :: Int -> Int -> CInt -> Int -> CursorType -> ToRender
updateListCursor d x y _ (CursorPointer tE) = addTexture renderEmpty d 0 $ DTexture t x' y w h Nothing
    where
        x' = fromIntegral $ x - 20
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
updateListCursor d x y tl (CursorRect c) = addRectangle renderEmpty d 0 $ DRectangle c x' (y - 3) w 20
    where
        x' = fromIntegral $ x - 5
        w = fromIntegral $ tl * 5 + 10


updateMenuListOptions :: [MenuAction] -> CInt -> CInt -> [TextDisplay]
updateMenuListOptions [] _ _ = []
updateMenuListOptions (h:tl) x y = dis : updateMenuListOptions tl x newY
    where
        newY = y + 20
        dis = TextDisplay str x y w 15 Blue
        (str, w) =
            let optText = menuOptionText h
                optLen = fromIntegral $ T.length optText
            in (optText, 5 * optLen)
