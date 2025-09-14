{-# LANGUAGE OverloadedStrings #-}

module Graphics.Draw
    ( updateBasicView
    , updateTimeoutView
    , updateGameMenu
    , updateGameView
    ) where

import Foreign.C.Types ( CInt )
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Maybe (isJust)

import OutputHandles.Types
import OutputHandles.Images
import OutputHandles.Text
import OutputHandles.Util
import Graphics.Types
import Graphics.Menu

import Debug.Trace

-- Turn the basic view into a render object to draw to the screen
updateBasicView :: Graphics -> Int -> BasicView a -> ToRender
updateBasicView gr d (BasicMenu m) = updateGameMenu gr d m
updateBasicView gr d (BasicTimeoutView tov) = updateTimeoutView gr d tov

updateTimeoutView :: Graphics -> Int -> TimeoutView a -> ToRender
updateTimeoutView gr d tov = updateGameView gr d (timeoutView tov)

-- Add the scroll rectangle visual on the side.
-- parameters
--  - render to add to
--  - depth for layering
--  - x start position for bar
--  - y start positing for scroll area
--  - height of the visible area
--  - the full height of the area if it wasn't scrolled
--  - height of step when you scroll down
--  - current offset (number of steps) down in the scroll
addScrollRects :: ToRender -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ToRender
addScrollRects r d x y fullHeight scrollHeight barHeight offH
    -- No scroll bar if the area fits
    | fullHeight < scrollHeight = r
    | otherwise = addRectangle (addRectangle r d 1 rect) d 2 rect2
    where
        maxBarY = y + scrollHeight - barHeight
        subStart = fromIntegral $ min (fromIntegral $ y + offH) maxBarY
        rect = DRectangle DarkGray (fromIntegral x) (fromIntegral (y - 1)) 4 (fromIntegral (scrollHeight + 2))
        rect2 = DRectangle Gray (fromIntegral x) subStart 4 (fromIntegral barHeight)


-- Update a view that has a scroll bar
updateGameViewScroll :: Graphics -> Int -> ViewScroll a -> ToRender
updateGameViewScroll gr d (ViewScroll subView off maxY step (ScrollData xStart yStart viewHeight scrollHeight barHeight maxOff)) = r'''
    where
        fs = graphicsFontSize gr
        r = updateGameView gr d subView
        offH = step * off
        r' = moveRenderY r (-offH)
        r'' = clipYRender fs r' yStart maxY
        r''' = addScrollRects r'' (d + 1) (xStart - 8) yStart viewHeight scrollHeight barHeight offH

-- Update a generic view
updateGameView :: Graphics -> Int -> View a -> ToRender
updateGameView gr d (View words imgs rs scrollM popM) = r'''
    where
        r = foldl (\rend td -> addText rend d 1 td) renderEmpty words
        r' = foldl (\rend t -> addTexture rend d 0 (toDraw t )) r imgs
        r'' = foldl (\rend (c, x, y, w, h) -> addRectangle rend d 1 (DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))) r' rs
        toDraw :: (Int, Int, Double, Image) -> DrawTexture
        toDraw (x, y, s, tE) =
            let (TextureInfo w h) = (graphicsTextures gr ! tE)
            in DTexture tE (fromIntegral x) (fromIntegral y)
                                        (floor ((fromIntegral w) * s)) (floor ((fromIntegral h) * s)) Nothing
        r''' = case scrollM of
                Nothing -> r''
                Just scroll -> r'' <> updateGameViewScroll gr d scroll

-- Add a popup menu to the render
addPopup :: Graphics -> Int -> ToRender -> MenuPopup a -> ToRender
addPopup gr d r (MenuPopup m x y w h c) = r'
    where
        r' = addRectangle r (d+1) 1 (DRectangle c (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))
            <> updateGameMenu gr (d+2) m


-- Translate a menu obeject into a render object to show it on screen
updateGameMenu :: Graphics -> Int -> Menu a -> ToRender
updateGameMenu gr d (Menu v opts) = updateGameView gr d v <> updateMenuOptions gr d opts

-- Translate the menu options into render object to draw
updateMenuOptions :: Graphics -> Int -> MenuOptions a -> ToRender
updateMenuOptions gr d mopts@(MenuOptions _ bdi p)
    | optionLength mopts == 0 = renderEmpty
    | otherwise =
        case menuOptions mopts of
            (SelOneListOpts oalOpt) -> updateSelOneListOptions gr d p bdi oalOpt
            (SelMultiListOpts mslOpt) -> updateMultiListOptions gr d p bdi mslOpt
            (ScrollListOpts slOpt) -> updateScrollListOptions gr d p bdi slOpt

-- Make a rectangle around text in the given space
getTextRectangle :: Color -> CInt -> CInt -> Int -> Int -> Int -> Int -> DrawRectangle
getTextRectangle c x y textL textH fSize sp = DRectangle c x' y' w h
    where
        xAdj = max 1 (fromIntegral fSize)
        yAdj = max 2 (min (floor (fromIntegral (sp - 1) / 2)) (fromIntegral (div (textH - 1) 2)))
        x' = fromIntegral $ x - xAdj
        y' = fromIntegral $ y - yAdj
        w = fromIntegral textL + (2 * xAdj)
        h = fromIntegral textH + (2 * yAdj)

-- Draw the menu options for menus that you only select one option at a time
updateSelOneListOptions :: Graphics -> Int -> Int -> BlockDrawInfo -> OneActionListOptions a -> ToRender
updateSelOneListOptions gr@(Graphics _ (fw, fh)) d pos (BlockDrawInfo x y s sp) (OALOpts ma curs) = r'
    where
        r = if pos >= 0 && pos < length ma then updateListCursor gr d oX yPos' cL (h - sp) s sp curs else renderEmpty
        r' = foldl (\rend td -> addText rend d 2 td) r $ updateMenuListOptions opts s h oX oY
        opts = (\mo -> (menuOptionText mo, if isJust $ menuNextState mo then Blue else Gray)) <$> ma
        yPos = y + (h * pos)
        yPos' = fromIntegral yPos
        oX = fromIntegral x
        oY = fromIntegral y
        opt = ma !! pos
        cL = ceiling $ fw * fromIntegral (s * T.length (menuOptionText opt))
        h = sp + ceiling (fh * fromIntegral s)

-- Draw the cursor texture or a rectangle around a given section of the screen based on text position / size
updateListCursor :: Graphics -> Int -> CInt -> CInt -> Int -> Int -> Int -> Int -> CursorType -> ToRender
updateListCursor gr d x y _ _ _ sp (CursorPointer t) = addTexture renderEmpty d 0 $ DTexture t x' y' w h Nothing
    where
        (TextureInfo tW tH) = graphicsTextures gr ! t
        x' = x - 20
        y' = y - 3
        w = fromIntegral tW
        h = fromIntegral tH
updateListCursor _ d x y tl th r sp (CursorRect c) = addRectangle renderEmpty d 0 rect
    where
        rect = getTextRectangle c x y tl th r sp

-- Draw the menu options for menus that you can select multiple options at a time
updateMultiListOptions :: Graphics -> Int -> Int -> BlockDrawInfo -> MultiSelectListOptions a -> ToRender
updateMultiListOptions (Graphics _ fs) d pos (BlockDrawInfo x y s sp) (MSLOpts mo _ _ backM) =
    case backM of
        Nothing -> updateSelectedOptions fs s sp renderEmpty pos 0 d mo' x' y'
        Just b -> updateSelectedOptions fs s sp renderEmpty pos 0 d (moBack b) x' y'
    where
        x' = fromIntegral x
        y' = fromIntegral y
        mo' = mo ++ [SelectOption "Continue" "" False False]
        moBack b = mo' ++ [SelectOption "Back" "" False False]

-- Differentiate between selected and unselected options and draw list of options given that info
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
                tlen = ceiling (fw * fromIntegral (T.length str * s))
                tH = fh * fromIntegral s
                td = TextDisplay str x yPos s Blue
                hlC
                    | cp == pos = Yellow
                    | changeable h = White
                    | otherwise = Gray
                tHI = ceiling tH
                yPos' = yPos + fromIntegral (sp + tHI)
                hRect = getTextRectangle hlC x yPos tlen tHI s sp

-- Update which items are selected in a multi-select list
updateMenuListOptions :: [(T.Text, Color)] -> Int -> Int -> CInt -> CInt -> [TextDisplay]
updateMenuListOptions opts s h x = updateMenuListOptions' opts
    where
        updateMenuListOptions' [] _ = []
        updateMenuListOptions' ((optText, col):tl) y = dis : updateMenuListOptions' tl newY
            where
                newY = y + fromIntegral h
                dis = TextDisplay optText x y s col

-- Draw menu options with columns and a button for each row
updateColumnButtonOptions :: Graphics -> Int -> Int -> BlockDrawInfo -> ColumnButtonOptions a -> (ToRender, Int)
updateColumnButtonOptions gr@(Graphics _ fs@(fw, fh)) d pos bdi@(BlockDrawInfo x y s sp) (CBOpts bt tw hds opts) = rend
    where
        rend = columnButtonOptionTexts gr pos d s sp tw bt hds opts (fromIntegral x) (fromIntegral y)


columnButtonOptionTexts :: Graphics -> Int -> Int -> Int -> Int -> Int -> T.Text -> [T.Text] -> [ColumnAction a] -> Int -> Int -> (ToRender, Int)
columnButtonOptionTexts (Graphics _ fs@(fw, fh)) p d s sp w butText headers opts x y = columnButtonOptionTexts' rend 0 opts (fromIntegral y + fromIntegral (sp + tH))
    where
        (lastX, rend) = foldl (\(tx, ls) t -> (tx + fromIntegral w, addText ls d 2 $ TextDisplay t tx (fromIntegral y) s White)) (fromIntegral x, mempty) headers
        tL = ceiling $ fw * fromIntegral (s * T.length butText)
        tH = ceiling (fh * fromIntegral s)
        yAdj = fromIntegral (sp + tH)
        columnButtonOptionTexts' :: ToRender -> Int -> [ColumnAction a] -> CInt -> (ToRender, Int)
        columnButtonOptionTexts' r _ [] yPos = (r, fromIntegral yPos)
        columnButtonOptionTexts' r n (h:tl) yPos = columnButtonOptionTexts' r'' (n + 1) tl newY
            where
                buttonColor = if p == n then (if isEnabled then Yellow else Red) else (if isEnabled then Green else Gray)
                newY = yPos + yAdj
                (maxX, r') = foldl columnDraw (fromIntegral x, r) $ colOptionTexts h
                r'' = addText (addRectangle r' d 1 $ getTextRectangle buttonColor lastX yPos tL tH s sp) d 2 (TextDisplay butText lastX yPos s (if isEnabled then White else Black))
                isEnabled = isJust $ colOptionAction h
                columnDraw (tx, ls) t = (tx + fromIntegral w, addText ls d 2 $ TextDisplay t tx (fromIntegral yPos) s Blue)


updateScrollListOptions :: Graphics -> Int -> Int -> BlockDrawInfo -> ScrollListOptions a -> ToRender
updateScrollListOptions gr@(Graphics _ fs@(fw, fh)) d p bdi@(BlockDrawInfo x y s sp) (SLOpts opts fixedOpts (Scroll mx off))
    | optCount <= mx = r `mappend` fixedR
    | otherwise = rend
    where
        p' = p - off
        h = sp + ceiling (fh * fromIntegral s)
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
        ((r, yEnd), cur) = case opts of
                    BasicSOALOpts (OALOpts oal c) -> ((updateSelOneListOptions gr d p' bdi $ OALOpts (take mx (drop off oal)) c, y + scrollHeight), c)
                    BasicMSLOpts mo@(MSLOpts msl _ _ _) -> ((updateMultiListOptions gr d p' bdi $ mo { mslOpts = take mx (drop off msl) }, y + scrollHeight), CursorRect White)
                    BasicCBOpts cbo@(CBOpts _ _  _ opts) -> (updateColumnButtonOptions gr d p' bdi $ cbo { colButOptActions = take mx (drop off opts) }, CursorRect White)
        fixedR = updateSelOneListOptions gr d (p - (end + off)) (bdi { blockY = yEnd })  $ OALOpts fixedOpts cur
        rend = addRectangle (addRectangle r d 1 rect `mappend` fixedR) d 2 rect2
