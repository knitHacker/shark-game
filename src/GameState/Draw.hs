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
        toDraw (x, y, tE) = DTexture (texture tE) (fromIntegral x) (fromIntegral y)
                                     (fromIntegral (textureWidth tE)) (fromIntegral (textureHeight tE)) Nothing

updateGameMenu :: FontSize -> Int -> Menu -> ToRender
updateGameMenu fs d (Menu v opts p) = updateGameView d v <> updateMenuOptions fs d p opts

updateMenuOptions :: FontSize -> Int -> Int -> MenuOptions -> ToRender
updateMenuOptions _ _ _ (SelOneListOpts (OALOpts _ _ _ _ [] _)) = renderEmpty
updateMenuOptions fs d p (SelOneListOpts oalOpt) = updateSelOneListOptions fs d p oalOpt
updateMenuOptions _ _ _ (SelMultiListOpts (MSLOpts _ _ _ _ [] _ _ _)) = renderEmpty
updateMenuOptions fs d p (SelMultiListOpts mslOpt) = updateMultiListOptions fs d p mslOpt


getTextRectangle :: Color -> CInt -> CInt -> Int -> Int -> Int -> Int -> DrawRectangle
getTextRectangle c x y textL textH fSize sp = DRectangle c x' y' w h
    where
        xAdj = max 1 (fromIntegral fSize) --(floor ((fromIntegral (2 * fSize)) / 2)))
        yAdj = max 1 (min (floor (fromIntegral (sp - 1) / 2)) (fromIntegral (div (textH - 1) 2)))
        x' = fromIntegral $ x - xAdj
        y' = fromIntegral $ y - yAdj
        w = fromIntegral textL + (2 * xAdj)
        h = fromIntegral textH + (2 * yAdj)

updateSelOneListOptions :: FontSize -> Int -> Int -> OneActionListOptions -> ToRender
updateSelOneListOptions _ _ _ (OALOpts _ _ _ _ [] _) = renderEmpty
updateSelOneListOptions (fw, fh) d pos (OALOpts x y s sp ma curs) = r'
    where
        r = updateListCursor d oX yPos' cL (h - sp) s sp curs
        r' = foldl (\rend td -> addText rend d 2 td) r $ updateMenuListOptions (menuOptionText <$> ma) s h oX oY
        yPos = y + (h * pos)
        yPos' = fromIntegral yPos
        oX = fromIntegral x
        oY = fromIntegral y
        opt = ma !! pos
        cL = floor $ fw * (fromIntegral (s * (T.length (menuOptionText opt))))
        h = sp + (floor $ fh * (fromIntegral s))

updateListCursor :: Int -> CInt -> CInt -> Int -> Int -> Int -> Int -> CursorType -> ToRender
updateListCursor d x y _ _ _ sp (CursorPointer tE) = addTexture renderEmpty d 0 $ DTexture t x' y' w h Nothing
    where
        x' = x - 20
        y' = y - 2
        t = texture tE
        tW = textureWidth tE
        tH = textureHeight tE
        w = fromIntegral tW
        h = fromIntegral tH
updateListCursor d x y tl th r sp (CursorRect c) = addRectangle renderEmpty d 0 rect
    where
        rect = getTextRectangle c x y tl th r sp


updateMultiListOptions :: FontSize -> Int -> Int -> MultiSelectListOptions -> ToRender
updateMultiListOptions fs d pos (MSLOpts x y s sp mo _ _ backM) =
    case backM of
        Nothing -> updateSelectedOptions fs s sp renderEmpty pos 0 d mo' x' y'
        Just b -> updateSelectedOptions fs s sp renderEmpty pos 0 d (moBack b) x' y'
    where
        x' = fromIntegral x
        y' = fromIntegral y
        mo' = mo ++ [SelectOption "Continue" "" False False]
        moBack b = mo' ++ [SelectOption "Back" "" False False]

updateSelectedOptions :: FontSize -> Int -> Int -> ToRender -> Int -> Int -> Int -> [SelectOption] -> CInt -> CInt -> ToRender
updateSelectedOptions (fw, fh) s sp r cp curp d opts x y = updateSelectedOptions' r curp opts y
    where
        updateSelectedOptions' :: ToRender -> Int -> [SelectOption] -> CInt -> ToRender
        updateSelectedOptions' rend _ [] _ = rend
        updateSelectedOptions' rend pos (h:tl) yPos = updateSelectedOptions' r'' (pos + 1) tl yPos'
            where
                r' = addText rend d 2 td
                r'' = if selectSelected h || cp == pos then addRectangle r' d 1 hRect else r'
                str = selectOptionText h
                tlen = floor (fw * (fromIntegral (T.length str * s)))
                tH = floor (fh * (fromIntegral s))
                td = TextDisplay str x yPos s Blue
                hlC = if cp == pos then Yellow else (if changeable h then White else Gray)
                yPos' = yPos + (fromIntegral (sp + tH))
                hRect = getTextRectangle hlC x yPos tlen tH s sp

updateMenuListOptions :: [T.Text] -> Int -> Int -> CInt -> CInt -> [TextDisplay]
updateMenuListOptions opts s h x y = updateMenuListOptions' opts y
    where
        updateMenuListOptions' [] _ = []
        updateMenuListOptions' (optText:tl) y = dis : updateMenuListOptions' tl newY
            where
                newY = y + (fromIntegral h)
                dis = TextDisplay optText x y s Blue
