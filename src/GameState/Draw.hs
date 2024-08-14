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
updateGameMenu d (Menu words imgs opts p) = r' <> updateMenuOptions d p opts
    where
        r = foldl (\rend td -> addText rend d 1 td) renderEmpty words
        r' = foldl (\rend t -> addTexture rend d 0 (toDraw t )) r imgs
        toDraw (x, y, tE) = DTexture (texture tE) (fromIntegral x) (fromIntegral y)
                                     (fromIntegral (textureWidth tE)) (fromIntegral (textureHeight tE)) Nothing

updateMenuOptions :: Int -> Int -> MenuOptions -> ToRender
updateMenuOptions _ _ (SelOneListOpts (OALOpts _ _ [] _)) = renderEmpty
updateMenuOptions d p (SelOneListOpts oalOpt) = updateSelOneListOptions d p oalOpt
updateMenuOptions _ _ (SelMultiListOpts (MSLOpts _ _ [] _ _ _)) = renderEmpty
updateMenuOptions d p (SelMultiListOpts mslOpt) = updateMultiListOptions d p mslOpt


updateSelOneListOptions :: Int -> Int -> OneActionListOptions -> ToRender
updateSelOneListOptions _ _ (OALOpts _ _ [] _) = renderEmpty
updateSelOneListOptions d pos (OALOpts x y ma curs) = r'
    where
        r = updateListCursor d x yPos' cL curs
        r' = foldl (\rend td -> addText rend d 2 td) r $ updateMenuListOptions (menuOptionText <$> ma) oX oY
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
        w = fromIntegral $ tl * 6 + 10


updateMultiListOptions :: Int -> Int -> MultiSelectListOptions -> ToRender
updateMultiListOptions d pos (MSLOpts x y mo _ _ backM) =
    case backM of
        Nothing -> updateSelectedOptions renderEmpty pos 0 d mo' x' y'
        Just b -> updateSelectedOptions renderEmpty pos 0 d (moBack b) x' y'
    where
        x' = fromIntegral x
        y' = fromIntegral y
        mo' = mo ++ [SelectOption "Continue" "" False False]
        moBack b = mo' ++ [SelectOption "Back" "" False False]

updateSelectedOptions :: ToRender -> Int -> Int -> Int -> [SelectOption] -> CInt -> CInt -> ToRender
updateSelectedOptions r _ _ _ [] _ _ = r
updateSelectedOptions r cp curp d (h:tl) x y = updateSelectedOptions r'' cp (curp + 1) d tl x (y + 20)
    where
        r' = addText r d 2 td
        r'' = if selectSelected h || cp == curp then addRectangle r' d 1 hRect else r'
        str = selectOptionText h
        tlen = fromIntegral $ T.length str
        td = TextDisplay str x y (tlen * 5) 15 Blue
        hlC = if cp == curp then Yellow else Gray
        hRect = DRectangle hlC (x - 5) (y - 2) (tlen * 6 + 10) 18

updateMenuListOptions :: [T.Text] -> CInt -> CInt -> [TextDisplay]
updateMenuListOptions [] _ _ = []
updateMenuListOptions (optText:tl) x y = dis : updateMenuListOptions tl x newY
    where
        newY = y + 20
        dis = TextDisplay optText x y w 15 Blue
        optLen = fromIntegral $ T.length optText
        w = 6 * optLen
