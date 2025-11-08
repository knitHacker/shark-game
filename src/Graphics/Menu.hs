module Graphics.Menu
    ( mkScrollView
    , selOneOpts
    , selMultOpts
    , scrollOpts
    , getNextMenu
    , getNextOption
    , optionLength
    , incrementMenuCursor
    , incrementMenuOpt
    , decrementMenuCursor
    , decrementMenuOpt
    , scrollView
    , getOptSize
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



--mkMenuPop :: [TextDisplay] -> [(Int, Int, Double, Image)] -> Maybe (ViewScroll a) -> MenuData a -> Maybe (MenuPopup a) -> Menu a
--mkMenuPop words images scrollM options popupM = Menu (View words images [] scrollM) options popupM Nothing


mkScrollView :: Graphics -> [TextDisplay] -> [(Int, Int, Double, Image)] -> Int -> Int -> Int -> Maybe (ViewScroll a)
mkScrollView graphics words images offset maxY step = ViewScroll v offset maxY step <$> sdM
    where
        v = View words images [] Nothing
        sdM = mkScrollData graphics v offset maxY step


mkScrollData :: Graphics -> View a -> Int -> Int -> Int -> Maybe ScrollData
mkScrollData gr v offset maxY step = mkScrollData' <$> getViewSize gr v
    where
        mkScrollData' ((startX, startY), (w, h)) =
            let h2 = maxY - startY
                h4 = floor $ (fromIntegral h2^2) / (fromIntegral h)
                maxStep = ceiling $ (fromIntegral (h2 - h4)) / (fromIntegral step)
            in ScrollData startX startY h h2 h4 maxStep

getViewSize :: Graphics -> View a -> Maybe ((Int, Int), (Int, Int))
getViewSize _ (View [] [] [] Nothing) = Nothing
getViewSize (Graphics tm fs) (View txts imgs rects Nothing) = Just ((x, y), (w, h))
    where
        imgRects = (\(x, y, s, tE) ->
                        let (TextureInfo tw th) = (tm ! tE)
                        in (x, y, round ((fromIntegral tw) * s), round ((fromIntegral th) * s))
                     ) <$> imgs
        txMinYM = getTextMinY txts
        txMaxYM = getTextMaxY fs txts
        txMinXM = getTextMinX txts
        txMaxXM = getTextMaxX fs txts
        rcMinYM = if null rects then Nothing else Just (minimum $ map (\(_, _, y, _, _) -> y) rects)
        rcMaxYM = if null rects then Nothing else Just (maximum $ map (\(_, _, y, _, h) -> y + h) rects)
        rcMinXM = if null rects then Nothing else Just (minimum $ map (\(_, x, _, _, _) -> x) rects)
        rcMaxXM = if null rects then Nothing else Just (maximum $ map (\(_, x, _, w, _) -> x + w) rects)
        imgMinYM = if null imgs then Nothing else Just (minimum $ map (\(_, y, _, _) -> y) imgs)
        imgMaxYM = if null imgs then Nothing else Just (maximum $ map (\(_, y, r, tE) -> let (TextureInfo _ th) = (tm ! tE)
                                                                                         in round (fromIntegral th * r) + y) imgs)
        imgMinXM = if null imgs then Nothing else Just (minimum $ map (\(x, _, _, _) -> x) imgs)
        imgMaxXM = if null imgs then Nothing else Just (maximum $ map (\(x, _, r, tE) -> let (TextureInfo tw _ ) = (tm ! tE)
                                                                                         in round (fromIntegral tw * r) + x) imgs)
        x = minimum $ catMaybes [imgMinXM, rcMinXM, txMinXM]
        y = minimum $ catMaybes [imgMinYM, rcMinYM, txMinYM]
        w = (maximum $ catMaybes [imgMaxXM, rcMaxXM, txMaxXM]) - x
        h = (maximum $ catMaybes [imgMaxYM, rcMaxYM, txMaxYM]) - y

selOneOpts :: Int -> Int -> Int -> Int -> [MenuAction a] -> CursorType -> Int -> MenuData a
selOneOpts x y s sp opts curs = MenuData (SelOneListOpts $ OALOpts opts curs) (BlockDrawInfo x y s sp)

selMultOpts :: Int -> Int -> Int -> Int -> [SelectOption]
            -> ([T.Text] -> Int -> a)
            -> Maybe ([T.Text] -> a)
            -> Maybe a -> Int -> MenuData a
selMultOpts x y s sp opts up act back = MenuData (SelMultiListOpts $ MSLOpts opts up act back) (BlockDrawInfo x y s sp)

scrollOpts :: Int -> Int -> Int -> Int -> BasicOption a -> [MenuAction a] -> Int -> Int -> MenuData a
scrollOpts x y s sp opts fixed maxScroll pos = MenuData (ScrollListOpts $ SLOpts opts fixed (Scroll maxScroll pos)) (BlockDrawInfo x y s sp) pos

getNextOption :: MenuData a -> Maybe a
getNextOption (MenuData (SelOneListOpts opts) _ pos) = getNextOALOpts opts pos
getNextOption (MenuData (SelMultiListOpts opts) _ pos) = getNextMSLOpts opts pos
getNextOption (MenuData (ScrollListOpts (SLOpts opts fixed _)) _ pos)
    | pos < optLen = getNextOpt opts pos
    | otherwise = menuNextState opt
    where
        optLen = getOptSize opts
        opt = fixed !! (pos - optLen)

getNextMenu :: Menu a -> Maybe a
getNextMenu (Menu _ (Just mp)) = getNextOption (popupOptions mp)
getNextMenu (Menu mo Nothing) = getNextOption mo

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
getNextOALOpts (OALOpts opts _) pos = menuNextState opt
    where
        opt = opts !! pos

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
optionLength (MenuData (SelOneListOpts opts) _ _) = length $ oalOpts opts
optionLength (MenuData (SelMultiListOpts opts) _ _) = 1 + length (mslOpts opts) + if isJust (mslBackActionM opts) then 1 else 0
optionLength (MenuData (ScrollListOpts opts) _ _) = length (sLFixedOpts opts) + case sLScrollOpts opts of
    BasicSOALOpts oal -> length $ oalOpts oal
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
incrementMenuOpt mo@(MenuData (ScrollListOpts sl@(SLOpts _ _ (Scroll mx off))) _ p)
    | p >= len - 1 = mo
    | p + off < end - 1 || off >= scrollLen - end = mo { cursorPosition = p + 1 }
    | otherwise = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off + 1) })
                                   , cursorPosition = p + 1 }
    where
        len = optionLength mo
        fxLen = length $ sLFixedOpts sl
        scrollLen = len - fxLen
        end = min mx len
incrementMenuOpt mo@(MenuData _ _ p)
    | p >= optionLength mo - 1 = mo
    | otherwise = mo { cursorPosition = p + 1 }

decrementMenuOpt :: MenuData a -> MenuData a
decrementMenuOpt mo@(MenuData (ScrollListOpts sl@(SLOpts _ _ (Scroll mx off))) _ p)
    | p == 0 = mo
    | p > off = mo { cursorPosition = p - 1 }
    | otherwise = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off - 1) })
                                   , cursorPosition = p - 1 }
decrementMenuOpt mo@(MenuData _ _ p)
    | p == 0 = mo
    | otherwise = mo { cursorPosition = p - 1 }


incrementMenuCursor :: Menu a -> Menu a
incrementMenuCursor m@(Menu _ (Just mp)) = m { popupMaybe = Just (mp { popupOptions = incrementMenuOpt (popupOptions mp) }) }
incrementMenuCursor m@(Menu mo Nothing) = m { options = incrementMenuOpt mo }

decrementMenuCursor :: Menu a -> Menu a
decrementMenuCursor m@(Menu _ (Just mp)) = m { popupMaybe = Just (mp { popupOptions = decrementMenuOpt (popupOptions mp) }) }
decrementMenuCursor m@(Menu mo@(MenuData (ScrollListOpts sl@(SLOpts _ _ (Scroll mx off))) _ p) Nothing)
    | p == 0 = m
    | p > off = m { options = mo { cursorPosition = p - 1 } }
    | otherwise = m { options = mo { menuOptions = ScrollListOpts (sl { sLScroll = Scroll mx (off - 1) })
                                   , cursorPosition = p - 1 }
                    }
decrementMenuCursor m@(Menu mo@(MenuData _ _ p) Nothing)
    | p == 0 = m
    | otherwise = m { options = mo { cursorPosition = p - 1 } }

scrollView :: ViewScroll a -> Int -> ViewScroll a
scrollView vs sAmt = vs { scrollOffset = newOffset }
    where
        sd = scrollData vs
        newOffset = max 0 $ min (scrollOffset vs - sAmt) (scrollMaxOffset sd)
