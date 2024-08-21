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

updateWindow :: (MonadIO m, ConfigsRead m, GameStateRead m, OutputRead m) => m (Maybe ToRender)
updateWindow = do
    outs <- getOutputs
    cfgs <- readConfigs
    gs <- readGameState
    if gameReDraw gs then return (Just (updateWindow' (getFontSize outs) (gameView gs))) else return Nothing


updateWindow' :: FontSize -> GameView -> ToRender
updateWindow' fs gv =
    case gv of
        (GameView _ m) -> updateGameMenu fs 0 m
        (OverlayMenu (Overlay x y w h c m) back) ->
            let backR = updateGameMenu fs 0 back
                bg = addRectangle renderEmpty 1 0 (DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
                frontR = updateGameMenu fs 2 m
            in backR <> bg <> frontR
        _ -> renderEmpty

updateGameMenu :: FontSize -> Int -> Menu -> ToRender
updateGameMenu fs d (Menu words imgs opts p) = r' <> updateMenuOptions fs d p opts
    where
        r = foldl (\rend td -> addText rend d 1 td) renderEmpty words
        r' = foldl (\rend t -> addTexture rend d 0 (toDraw t )) r imgs
        toDraw (x, y, tE) = DTexture (texture tE) (fromIntegral x) (fromIntegral y)
                                     (fromIntegral (textureWidth tE)) (fromIntegral (textureHeight tE)) Nothing

updateMenuOptions :: FontSize -> Int -> Int -> MenuOptions -> ToRender
updateMenuOptions _ _ _ (SelOneListOpts (OALOpts _ _ _ _ [] _)) = renderEmpty
updateMenuOptions fs d p (SelOneListOpts oalOpt) = updateSelOneListOptions fs d p oalOpt
updateMenuOptions _ _ _ (SelMultiListOpts (MSLOpts _ _ _ _ [] _ _ _)) = renderEmpty
updateMenuOptions fs d p (SelMultiListOpts mslOpt) = updateMultiListOptions fs d p mslOpt


updateSelOneListOptions :: FontSize -> Int -> Int -> OneActionListOptions -> ToRender
updateSelOneListOptions _ _ _ (OALOpts _ _ _ _ [] _) = renderEmpty
updateSelOneListOptions (fw, fh) d pos (OALOpts x y s sp ma curs) = r'
    where
        r = updateListCursor d x yPos' cL curs
        r' = foldl (\rend td -> addText rend d 2 td) r $ updateMenuListOptions (menuOptionText <$> ma) s h oX oY
        yPos = y + (h * pos)
        yPos' = fromIntegral yPos
        oX = fromIntegral x
        oY = fromIntegral y
        opt = ma !! pos
        cL = floor $ fw * (fromIntegral (s * (T.length (menuOptionText opt))))
        h = sp + (floor $ fh * (fromIntegral s))

updateListCursor :: Int -> Int -> CInt -> Int -> CursorType -> ToRender
updateListCursor d x y _ (CursorPointer tE) = addTexture renderEmpty d 0 $ DTexture t x' (y - 1) w h Nothing
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
        w = fromIntegral $ tl + 10


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
        updateSelectedOptions' rend pos (h:tl) yPos = updateSelectedOptions' r'' (pos + 1) tl (yPos + (fromIntegral tH))
            where
                r' = addText rend d 2 td
                r'' = if selectSelected h || cp == curp then addRectangle r' d 1 hRect else r'
                str = selectOptionText h
                tlen = floor (fw * (fromIntegral (T.length str * s)))
                tH = floor (fh * (fromIntegral s)) + sp
                td = TextDisplay str x y s Blue
                hlC = if cp == curp then Yellow else Gray
                hRect = DRectangle hlC (x - 5) (y - 2) (tlen + 10) (fromIntegral tH)

updateMenuListOptions :: [T.Text] -> Int -> Int -> CInt -> CInt -> [TextDisplay]
updateMenuListOptions opts s h x y = updateMenuListOptions' opts y
    where
        updateMenuListOptions' [] _ = []
        updateMenuListOptions' (optText:tl) y = dis : updateMenuListOptions' tl newY
            where
                newY = y + (fromIntegral h)
                dis = TextDisplay optText x y s Blue
