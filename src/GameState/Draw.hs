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
import GameState
import GameState.Types
import OutputHandles.Types
import OutputHandles.Draw
import InputState

import Debug.Trace
import GHC.Real (fromIntegral)
import GameState.Collision.BoundBox

type FontSize = (Double, Double)

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m, OutputRead m) => m ToRender
updateWindow = do
    outs <- getOutputs
    cfgs <- readConfigs
    gs <- readGameState
    case gameLastDraw gs of
        Just r -> return r
        Nothing -> return $ updateWindow' (getFontSize outs) (gameView gs)


updateWindow' :: FontSize -> GameView -> ToRender
updateWindow' fs gv =
    case gv of
        (GameMenu _ m) -> updateGameMenu fs 0 m
        (OverlayMenu (Overlay x y w h c m) back) ->
            let backR = updateBasicView fs 0 back
                bg = addRectangle renderEmpty 1 0 (DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
                frontR = updateGameMenu fs 2 m
            in backR <> bg <> frontR
        (GameTimeout _ tov) -> updateTimeoutView 0 tov
        _ -> renderEmpty


updateBasicView :: FontSize -> Int -> BasicView -> ToRender
updateBasicView fs d (BasicMenu m) = updateGameMenu fs d m
updateBasicView _ d (BasicTimeoutView tov) = updateTimeoutView d tov

updateTimeoutView :: Int -> TimeoutView -> ToRender
updateTimeoutView d tov = updateGameView d (timeoutView tov)

updateGameView :: Int -> View -> ToRender
updateGameView d (View words imgs rs) = r''
    where
        r = foldl (\rend td -> addText rend d 1 td) renderEmpty words
        r' = foldl (\rend t -> addTexture rend d 0 (toDraw t )) r imgs
        r'' = foldl (\rend (c, x, y, w, h) -> addRectangle rend d 1 (DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))) r' rs
        toDraw :: (Int, Int, Double, TextureEntry) -> DrawTexture
        toDraw (x, y, s, tE) = DTexture (texture tE) (fromIntegral x) (fromIntegral y)
                                        (floor ((fromIntegral (textureWidth tE)) * s)) (floor ((fromIntegral (textureHeight tE)) * s)) Nothing

updateGameMenu :: FontSize -> Int -> Menu -> ToRender
updateGameMenu fs d (Menu v opts) = updateGameView d v <> updateMenuOptions fs d opts

updateMenuOptions :: FontSize -> Int -> MenuOptions -> ToRender
updateMenuOptions fs d mopts@(MenuOptions _ bdi p)
    | optionLength mopts == 0 = renderEmpty
    | otherwise =
        case menuOptions mopts of
            (SelOneListOpts oalOpt) -> updateSelOneListOptions fs d p bdi oalOpt
            (SelMultiListOpts mslOpt) -> updateMultiListOptions fs d p bdi mslOpt
            (ScrollListOpts slOpt) -> updateScrollListOptions fs d p bdi slOpt

getTextRectangle :: Color -> CInt -> CInt -> Int -> Int -> Int -> Int -> DrawRectangle
getTextRectangle c x y textL textH fSize sp = DRectangle c x' y' w h
    where
        xAdj = max 1 (fromIntegral fSize)
        yAdj = max 2 (min (floor (fromIntegral (sp - 1) / 2)) (fromIntegral (div (textH - 1) 2)))
        x' = fromIntegral $ x - xAdj
        y' = fromIntegral $ y - yAdj
        w = fromIntegral textL + (2 * xAdj)
        h = fromIntegral textH + (2 * yAdj)

updateSelOneListOptions :: FontSize -> Int -> Int -> BlockDrawInfo -> OneActionListOptions -> ToRender
updateSelOneListOptions (fw, fh) d pos (BlockDrawInfo x y s sp) (OALOpts ma curs) = r'
    where
        r = if pos >= 0 && pos < length ma then updateListCursor d oX yPos' cL (h - sp) s sp curs else renderEmpty
        r' = foldl (\rend td -> addText rend d 2 td) r $ updateMenuListOptions opts s h oX oY
        opts = (\mo -> (menuOptionText mo, if menuOptionEnabled mo then Blue else Gray)) <$> ma
        yPos = y + (h * pos)
        yPos' = fromIntegral yPos
        oX = fromIntegral x
        oY = fromIntegral y
        opt = ma !! pos
        cL = floor $ fw * fromIntegral (s * T.length (menuOptionText opt))
        h = sp + floor (fh * fromIntegral s)

updateListCursor :: Int -> CInt -> CInt -> Int -> Int -> Int -> Int -> CursorType -> ToRender
updateListCursor d x y _ _ _ sp (CursorPointer tE) = addTexture renderEmpty d 0 $ DTexture t x' y' w h Nothing
    where
        x' = x - 20
        y' = y - 3
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
updateListCursor d x y tl th r sp (CursorRect c) = addRectangle renderEmpty d 0 rect
    where
        rect = getTextRectangle c x y tl th r sp

updateMultiListOptions :: FontSize -> Int -> Int -> BlockDrawInfo -> MultiSelectListOptions -> ToRender
updateMultiListOptions fs d pos (BlockDrawInfo x y s sp) (MSLOpts mo _ _ backM) =
    case backM of
        Nothing -> updateSelectedOptions fs s sp renderEmpty pos 0 d mo' x' y'
        Just b -> updateSelectedOptions fs s sp renderEmpty pos 0 d (moBack b) x' y'
    where
        x' = fromIntegral x
        y' = fromIntegral y
        mo' = mo ++ [SelectOption "Continue" "" False False]
        moBack b = mo' ++ [SelectOption "Back" "" False False]

updateSelectedOptions :: FontSize -> Int -> Int -> ToRender -> Int -> Int -> Int -> [SelectOption] -> CInt -> CInt -> ToRender
updateSelectedOptions (fw, fh) s sp r cp curp d opts x = updateSelectedOptions' r curp opts
    where
        updateSelectedOptions' :: ToRender -> Int -> [SelectOption] -> CInt -> ToRender
        updateSelectedOptions' rend _ [] _ = rend
        updateSelectedOptions' rend pos (h:tl) yPos = updateSelectedOptions' r'' (pos + 1) tl yPos'
            where
                r' = addText rend d 2 td
                r'' = if selectSelected h || cp == pos then addRectangle r' d 1 hRect else r'
                str = selectOptionText h
                tlen = floor (fw * fromIntegral (T.length str * s))
                tH = floor (fh * fromIntegral s)
                td = TextDisplay str x yPos s Blue
                hlC
                    | cp == pos = Yellow
                    | changeable h = White
                    | otherwise = Gray
                yPos' = yPos + fromIntegral (sp + tH)
                hRect = getTextRectangle hlC x yPos tlen tH s sp

updateMenuListOptions :: [(T.Text, Color)] -> Int -> Int -> CInt -> CInt -> [TextDisplay]
updateMenuListOptions opts s h x = updateMenuListOptions' opts
    where
        updateMenuListOptions' [] _ = []
        updateMenuListOptions' ((optText, col):tl) y = dis : updateMenuListOptions' tl newY
            where
                newY = y + fromIntegral h
                dis = TextDisplay optText x y s col

updateScrollListOptions :: FontSize -> Int -> Int -> BlockDrawInfo -> ScrollListOptions -> ToRender
updateScrollListOptions fs@(fw, fh) d p bdi@(BlockDrawInfo x y s sp) (SLOpts opts fixedOpts (Scroll mx off))
    | optCount <= mx = r `mappend` fixedR
    | otherwise = rend
    where
        p' = p - off
        h = sp + floor (fh * fromIntegral s)
        scrollHeight = h * end
        optCount = getOptSize opts
        end = min mx optCount
        indicatorHeight = fromIntegral (scrollHeight - sp)
        moveAmt = indicatorHeight `div` fromIntegral optCount
        darkHeight = moveAmt * fromIntegral end
        rx = fromIntegral (x - 10)
        ry = fromIntegral y + (fromIntegral off * moveAmt)
        -- height is recalculated because of rounding errors with division with integers
        rect = DRectangle DarkGray rx (fromIntegral y - 1) 4 (moveAmt * fromIntegral optCount + 2)
        rect2 = DRectangle Gray rx ry 4 darkHeight
        (r, cur) = case opts of
                    BasicSOALOpts (OALOpts oal c) -> (updateSelOneListOptions fs d p' bdi $ OALOpts (take mx (drop off oal)) c, c)
                    BasicMSLOpts mo@(MSLOpts msl _ _ _) -> (updateMultiListOptions fs d p' bdi $ mo { mslOpts = take mx (drop off msl) }, CursorRect White)
        fixedR = updateSelOneListOptions fs d (p - (end + off)) (bdi { blockY = y + scrollHeight })  $ OALOpts fixedOpts cur
        rend = addRectangle (addRectangle r d 1 rect `mappend` fixedR) d 2 rect2
