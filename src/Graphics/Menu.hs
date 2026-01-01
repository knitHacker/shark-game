{-# LANGUAGE TupleSections #-}

module Graphics.Menu
    ( mkScrollView
    , selOneOpts
    , selMultOpts
    , scrollOpts
    , scrollOptsBasic
    , resizingScrollOpts
    , getNextMenu
    , getNextOption
    , getBackOption
    , optionLength
    , incrementMenuCursor
    , incrementMenuOpt
    , decrementMenuCursor
    , decrementMenuOpt
    , scrollView
    , getOptSize
    , getTextureSize
    , textView
    , getOptionHeight
    ) where

import qualified Data.Text as T
import Data.Map.Strict ((!))
import Data.Maybe (isJust, catMaybes)

import OutputHandles.Types
import OutputHandles.Text
import Graphics.Types
import Graphics.Animation
import InputState
import Data.IntMap (update)

import Debug.Trace
import System.Console.GetOpt (getOpt)

textView :: [TextDisplay] -> View a
textView words = View ((,0) <$> words) [] [] [] Nothing

mkScrollView :: Graphics -> [TextDisplay] -> [(Int, Int, Double, Image)] -> Int -> Int -> Int -> Maybe (ViewScroll a)
mkScrollView graphics words images offset maxY step = ViewScroll v offset maxY step <$> sdM
    where
        v = View ((,0) <$> words) ((\ (x, y, s, tE) -> IPlace x y s tE 1) <$> images) [] [] Nothing
        sdM = mkScrollData graphics v offset maxY step


mkScrollData :: Graphics -> View a -> Int -> Int -> Int -> Maybe ScrollData
mkScrollData gr v offset maxY step = mkScrollData' <$> getViewSize gr v
    where
        mkScrollData' ((startX, startY), (w, h)) =
            let h2 = maxY - startY
                h4 = floor $ (fromIntegral h2^2) / (fromIntegral h)
                -- Maximum scroll should allow us to see the bottom of the content
                maxScrollPixels = max 0 (h - h2)
                maxStep = ceiling $ fromIntegral maxScrollPixels / fromIntegral step
            in ScrollData startX startY h h2 h4 maxStep

getTextureSize :: TextureInfo -> (Int, Int)
getTextureSize (ImageCfg (ImageInfo sx sy)) = (sx, sy)
getTextureSize (AnimationCfg (AnimationInfo sx sy _ _)) = (sx, sy)

-- Calculate the height of a menu option given BlockDrawInfo and font size
-- This matches the calculation used in Graphics.Draw
getOptionHeight :: FontSize -> Int -> Int -> Int
getOptionHeight (_, fh) s sp = sp + ceiling (fh * fromIntegral s)

getViewSize :: Graphics -> View a -> Maybe ((Int, Int), (Int, Int))
getViewSize _ (View [] [] [] [] Nothing) = Nothing
getViewSize (Graphics tm _ fs _ _) (View txts imgs ans rects Nothing) = Just ((x, y), (w, h))
    where
        imgRects = (\(IPlace x y s tE _) ->
                        let (tw, th) = getTextureSize $ ImageCfg $ tm ! tE
                        in (x, y, round ((fromIntegral tw) * s), round ((fromIntegral th) * s))
                     ) <$> imgs
        txMinYM = getTextMinY $ fst <$> txts
        txMaxYM = getTextMaxY fs $ fst <$> txts
        txMinXM = getTextMinX $ fst <$> txts
        txMaxXM = getTextMaxX fs $ fst <$> txts
        rcMinYM = if null rects then Nothing else Just (minimum $ map (\(_, _, y, _, _, _) -> y) rects)
        rcMaxYM = if null rects then Nothing else Just (maximum $ map (\(_, _, y, _, h, _) -> y + h) rects)
        rcMinXM = if null rects then Nothing else Just (minimum $ map (\(_, x, _, _, _, _) -> x) rects)
        rcMaxXM = if null rects then Nothing else Just (maximum $ map (\(_, x, _, w, _, _) -> x + w) rects)
        imgMinYM = if null imgs then Nothing else Just (minimum $ map (\(IPlace _ y _ _ _) -> y) imgs)
        imgMaxYM = if null imgs then Nothing else Just (maximum $ map (\(IPlace _ y r tE _) -> let (_, th) = getTextureSize $ ImageCfg $ tm ! tE
                                                                                         in round (fromIntegral th * r) + y) imgs)
        imgMinXM = if null imgs then Nothing else Just (minimum $ map (\(IPlace x _ _ _ _) -> x) imgs)
        imgMaxXM = if null imgs then Nothing else Just (maximum $ map (\(IPlace x _ r tE _) -> let (tw, _) = getTextureSize $ ImageCfg $ tm ! tE
                                                                                         in round (fromIntegral tw * r) + x) imgs)
        x = minimum $ catMaybes [imgMinXM, rcMinXM, txMinXM]
        y = minimum $ catMaybes [imgMinYM, rcMinYM, txMinYM]
        w = (maximum $ catMaybes [imgMaxXM, rcMaxXM, txMaxXM]) - x
        h = (maximum $ catMaybes [imgMaxYM, rcMaxYM, txMaxYM]) - y

selOneOpts :: Int -> Int -> Int -> Int -> [MenuAction a] -> Maybe (MenuAction a) -> CursorType -> Int -> MenuData a
selOneOpts x y s sp opts backM curs = MenuData (SelOneListOpts $ OALOpts opts backM Nothing curs) (BlockDrawInfo x y s sp)

selMultOpts :: Int -> Int -> Int -> Int -> [SelectOption]
            -> ([T.Text] -> Int -> a)
            -> Maybe ([T.Text] -> a)
            -> Maybe a -> Int -> MenuData a
selMultOpts x y s sp opts up act back = MenuData (SelMultiListOpts $ MSLOpts opts up act back) (BlockDrawInfo x y s sp)

scrollOpts :: Int -> Int -> Int -> Int -> OneActionListOptions a -> Maybe (MenuAction a) -> [MenuAction a] -> Int -> Int -> MenuData a
scrollOpts x y s sp opts backM fixed maxScroll pos = MenuData (ScrollListOpts $ SLOpts (BasicSOALOpts opts) fixed backM (Scroll maxScroll initialOffset)) (BlockDrawInfo x y s sp) pos
    where
        -- Calculate initial scroll offset to ensure cursor position is visible
        -- If cursor is within the first maxScroll items, no offset needed
        -- Otherwise, offset so the cursor is visible (centered when possible)
        initialOffset = if pos < maxScroll then 0 else max 0 (pos - maxScroll + 1)

-- Version that takes BasicOption directly for non-OneActionListOptions cases
scrollOptsBasic :: Int -> Int -> Int -> Int -> BasicOption a -> Maybe (MenuAction a) -> [MenuAction a] -> Int -> Int -> MenuData a
scrollOptsBasic x y s sp opts backM fixed maxScroll pos = MenuData (ScrollListOpts $ SLOpts opts fixed backM (Scroll maxScroll initialOffset)) (BlockDrawInfo x y s sp) pos
    where
        initialOffset = if pos < maxScroll then 0 else max 0 (pos - maxScroll + 1)

resizingScrollOpts :: Graphics -> Int -> Int -> Int -> Int -> Int -> OneActionListOptions a -> Maybe (MenuAction a) -> [MenuAction a] -> Int -> MenuData a
resizingScrollOpts gr margin x y s sp opts backM fixed pos = MenuData (ScrollListOpts $ SLOpts (BasicSOALOpts opts) fixed backM (Scroll maxScroll initialOffset)) (BlockDrawInfo x y s sp) pos
    where
        windowHeight = graphicsWindowHeight gr
        fontSize = graphicsFontSize gr
        availableHeight = windowHeight - y - margin
        optionHeight = getOptionHeight fontSize s sp
        additionalOptsCount = length fixed + (if isJust backM then 1 else 0)
        maxVisibleOptions = max 1 (availableHeight `div` optionHeight)
        maxScroll = max 1 (maxVisibleOptions - additionalOptsCount)
        scrollableCount = length (oalOpts opts)
        -- Calculate initial scroll offset to ensure cursor position is visible
        -- If cursor is in fixed options area, clamp to show end of scrollable list
        -- If cursor is within the first maxScroll items, no offset needed
        -- Otherwise, offset so the cursor is visible (centered when possible)
        scrollPos = min pos (scrollableCount - 1)
        initialOffset = if scrollPos < maxScroll then 0 else max 0 (scrollPos - maxScroll + 1)



getNextOption :: MenuData a -> Maybe a
getNextOption (MenuData (SelOneListOpts opts) _ pos) = getNextOALOpts opts pos
getNextOption (MenuData (SelMultiListOpts opts) _ pos) = getNextMSLOpts opts pos
getNextOption (MenuData (ScrollListOpts (SLOpts opts fixed bM _)) _ pos)
    | pos < optLen = getNextOpt opts pos
    | pos - optLen < length fixed = menuNextState (fixed !! (pos - optLen))
    | otherwise = bM >>= menuNextState
    where
        optLen = getOptSize opts

getNextMenu :: Menu a -> Maybe a
getNextMenu (Menu _ (Just mp)) = getNextOption (popupOptions mp)
getNextMenu (Menu mo Nothing) = getNextOption mo

getBackOption :: Menu a -> Maybe a
getBackOption (Menu _ (Just mp)) = getBackOptionFromData (popupOptions mp)
getBackOption (Menu mo Nothing) = getBackOptionFromData mo

getBackOptionFromData :: MenuData a -> Maybe a
getBackOptionFromData (MenuData (SelOneListOpts (OALOpts _ (Just backOpt) _ _)) _ _) = menuNextState backOpt
getBackOptionFromData (MenuData (ScrollListOpts (SLOpts _ _ (Just backOpt) _)) _ _) = menuNextState backOpt
getBackOptionFromData (MenuData (SelMultiListOpts (MSLOpts _ _ _ backOptM)) _ _) = backOptM
getBackOptionFromData _ = Nothing

getBackOpt :: BasicOption a -> Maybe a
getBackOpt (BasicSOALOpts opts) = getBackOALOpts opts
getBackOpt _ = Nothing

getBackOALOpts :: OneActionListOptions a -> Maybe a
getBackOALOpts (OALOpts opts backM _ _) =
    case backM of
        Just backOpt -> menuNextState backOpt
        Nothing -> Nothing

getNextOpt :: BasicOption a -> Int -> Maybe a
getNextOpt (BasicSOALOpts opts) pos = getNextOALOpts opts pos
getNextOpt (BasicMSLOpts opts) pos = getNextMSLOpts opts pos
getNextOpt (BasicCBOpts opts) pos = getNextCBOpts opts pos
getNextOpt _ _ = Nothing

getOptSize :: BasicOption a -> Int
getOptSize (BasicSOALOpts opts) = length $ oalOpts opts
getOptSize (BasicMSLOpts opts) = length $ mslOpts opts
getOptSize (BasicCBOpts opts) = length $ colButOptActions opts
getOptSize (BasicTextOpts to) = length $ textOptionTexts to

getNextOALOpts :: OneActionListOptions a -> Int -> Maybe a
getNextOALOpts (OALOpts opts Nothing _ _) pos
    | pos < length opts = menuNextState (opts !! pos)
    | otherwise = Nothing
getNextOALOpts (OALOpts opts (Just back) _ _) pos
    | pos < length opts = menuNextState (opts !! pos)
    | pos == length opts = menuNextState back
    | otherwise = Nothing

getNextMSLOpts :: MultiSelectListOptions a -> Int -> Maybe a
getNextMSLOpts opts pos
    | pos < len = Just $ mslAction opts selected pos
    | len == pos = mslContinueAction opts <*> Just selected
    | otherwise = case mslBackActionM opts of
                    Nothing -> Just $ mslAction opts selected pos
                    Just back -> Just back
    where
        len = length (mslOpts opts)
        opts' = toggleMultiOption opts pos
        selected = selectKey <$> filter selectSelected (mslOpts opts')

getNextCBOpts :: ColumnButtonOptions a -> Int -> Maybe a
getNextCBOpts (CBOpts _ _  _ opts) pos
    | pos >= length opts = Nothing
    | isJust $ colOptionAction (opts !! pos) = colOptionAction (opts !! pos)
    | otherwise = Nothing

optionLength :: MenuData a -> Int
optionLength (MenuData (SelOneListOpts opts) _ _) = length (oalOpts opts) + if isJust (oalBackOptM opts) then 1 else 0
optionLength (MenuData (SelMultiListOpts opts) _ _) = 1 + length (mslOpts opts) + if isJust (mslBackActionM opts) then 1 else 0
optionLength (MenuData (ScrollListOpts opts) _ _) = length (sLFixedOpts opts) + (if isJust (sLBackOptM opts) then 1 else 0) + case sLScrollOpts opts of
    BasicSOALOpts oal -> length (oalOpts oal) + if isJust (oalBackOptM oal) then 1 else 0
    BasicMSLOpts msl -> 1 + length (mslOpts msl) + if isJust (mslBackActionM msl) then 1 else 0
    BasicCBOpts cb -> length $ colButOptActions cb
    BasicTextOpts to -> length $ textOptionTexts to

toggleMultiOption :: MultiSelectListOptions a -> Int -> MultiSelectListOptions a
toggleMultiOption opt pos = opt { mslOpts = (\(n, o) -> if n == pos then toggle o else o) <$> zip [0..] (mslOpts opt)}
    where
        toggle (SelectOption t k _ _ True) = SelectOption t k False False True
        toggle sopt@(SelectOption t k _ False _) = sopt
        toggle (SelectOption t k sel _ _ ) = SelectOption t k (not sel) True False

incrementMenuOpt :: MenuData a -> MenuData a
incrementMenuOpt mo@(MenuData (ScrollListOpts sl@(SLOpts opts _ _ (Scroll mx off))) _ p)
    | p >= len - 1 = mo
    | p >= scrollLen = mo { cursorPosition = p + 1 }  -- In fixed/back area, just move cursor
    | p + off < end - 1 || off >= scrollLen - end = mo { cursorPosition = p + 1 }
    | otherwise = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off + 1) })
                                   , cursorPosition = p + 1 }
    where
        len = optionLength mo
        scrollLen = getOptSize opts  -- Only count scrollable options
        end = min mx scrollLen
incrementMenuOpt mo@(MenuData _ _ p)
    | p >= optionLength mo - 1 = mo
    | otherwise = mo { cursorPosition = p + 1 }

decrementMenuOpt :: MenuData a -> MenuData a
decrementMenuOpt mo@(MenuData (ScrollListOpts sl@(SLOpts opts _ _ (Scroll mx off))) _ p)
    | p == 0 = mo
    | p >= scrollLen = mo { cursorPosition = p - 1 }  -- In fixed/back area, just move cursor
    | p > off = mo { cursorPosition = p - 1 }
    | otherwise = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off - 1) })
                                   , cursorPosition = p - 1 }
    where
        scrollLen = getOptSize opts
decrementMenuOpt mo@(MenuData _ _ p)
    | p == 0 = mo
    | otherwise = mo { cursorPosition = p - 1 }

incrementMenuCursor :: Menu a -> Either (Menu a) a
incrementMenuCursor m@(Menu _ (Just mp)) = Left $ m { popupMaybe = Just (mp { popupOptions = incrementMenuOpt (popupOptions mp) }) }
incrementMenuCursor m@(Menu md@(MenuData (SelOneListOpts opt@(OALOpts _ _ (Just update) _)) _ cp) _)
    | optLen >= cp + 1 = Left m
    | otherwise = Right $ update $ cp + 1
    where
        optLen = optionLength md
incrementMenuCursor m@(Menu md@(MenuData (ScrollListOpts (SLOpts (BasicSOALOpts (OALOpts _ _ (Just update) _)) _ _ _)) _ cp) Nothing)
    | cp + 1 >= optionLength md = Left m
    | otherwise = Right $ update $ cp + 1
incrementMenuCursor m@(Menu mo Nothing) = Left $ m { options = incrementMenuOpt mo }

decrementMenuCursor :: Menu a -> Either (Menu a) a
decrementMenuCursor m@(Menu _ (Just mp)) = Left $ m { popupMaybe = Just (mp { popupOptions = decrementMenuOpt (popupOptions mp) }) }
decrementMenuCursor m@(Menu md@(MenuData (SelOneListOpts opt@(OALOpts _ _ (Just update) _)) _ cp) _)
    | cp == 0 = Left m
    | otherwise = Right $ update $ cp - 1
decrementMenuCursor m@(Menu md@(MenuData (ScrollListOpts (SLOpts (BasicSOALOpts (OALOpts _ _ (Just update) _)) _ _ _)) _ cp) Nothing)
    | cp == 0 = Left m
    | otherwise = Right $ update $ cp - 1
decrementMenuCursor m@(Menu mo@(MenuData (ScrollListOpts sl@(SLOpts _ _ _ (Scroll mx off))) _ p) Nothing)
    | p == 0 = Left m
    | p > off = Left $ m { options = mo { cursorPosition = p - 1 } }
    | otherwise = Left $ m { options = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off - 1) })
                                   , cursorPosition = p - 1 }
                    }
decrementMenuCursor m@(Menu mo@(MenuData _ _ p) Nothing)
    | p == 0 = Left m
    | otherwise = Left $ m { options = mo { cursorPosition = p - 1 } }

scrollView :: ViewScroll a -> Int -> ViewScroll a
scrollView vs sAmt = vs { scrollOffset = newOffset }
    where
        sd = scrollData vs
        newOffset = max 0 $ min (scrollOffset vs - sAmt) (scrollMaxOffset sd)
