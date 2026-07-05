{-# LANGUAGE OverloadedStrings #-}

module GameState.Util
    ( shouldAnimationStep
    , getAnimationStep
    , pauseOverlay
    , getPauseEnterAction
    , getPauseMoveAction
    , simpleUpdate
    , updateMenuHighlight
    , withPauseUpdate
    , openPauseMenu
    , stepInputUpdate
    , stepTransition
    , updateDefSomeMenu
    , updateDefaultMenu
    , moveMenuPos
    , initSimpleStateInfo
    , initMenuStateInfo
    , menuGetData
    , menuGetPause
    , menuMoveUp
    , menuMoveDown
    , menuUpdateSel
    , menuUpdateData
    , menuInfoThink
    , menuInfoIntThink
    , menuInfoUpdate
    , simpleInfoPaused
    , menuInfoPaused
    , simpleInfoThink
    , expandingCenterTable
    , withPauseApply
    , menuInfoApply
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)

import InputState
import Graphics.Types
import Graphics.Asset
import GameState.Types
import Graphics.TextUtil
import OutputHandles.Types
import SaveData


expandingCenterTable :: Graphics -> Resize -> [(T.Text, Color)] -> [([(T.Text, Color)], Int)] -> Int -> Int -> Int -> Int -> Int -> Int -> Asset
expandingCenterTable gr rsFn headers items xMinStart xEndOff maxSp yStart ySp layer =
    Asset assObj (startX assObj gr) yStart layer True (Just assRs)
    where
        allRows = case (headers, items) of
            ([], _) -> items
            (_, (_, fs) : _) -> (headers, fs) : items
            _ -> [(headers, 3)]
        assObj = spacedCols colsBase gr
        colsBase = toTable allRows ySp
        assRs ass gr' =
            let newAss = ass { object = spacedCols (object ass) gr' }
            in rsFn (newAss { assetX = startX (object newAss) gr' }) gr'
        spacedCols (AssetStacked colsObj) gr' =
            let colsObj' = AssetStacked $ colsObj { stackSpace = 0 }
                colNum = length $ stackItems colsObj
                space = graphicsWindowWidth gr' - xMinStart - xEndOff
                colWidth = assetObjWidth gr' colsObj'
                sp = min maxSp $ max 1 $ (space - colWidth) `div` (colNum - 1)
            in AssetStacked $ colsObj { stackSpace = sp }
        spacedCols obj _ = obj
        startX assO gr' =
            let objWidth = assetObjWidth gr' assO
                space = graphicsWindowWidth gr' - xMinStart - xEndOff
            in xMinStart + (space - objWidth) `div` 2


toTable :: [([(T.Text, Color)], Int)] -> Int -> AssetObj
toTable rows sp = AssetStacked (AssetStack StackHorizontal ((\col -> StackItem col 0 0) <$> toStack <$> cols) 1)
    where
        cols = L.transpose $ (\(items, fs) -> (\(t, c) -> (t, c, fs)) <$> items) <$> rows
        toStack ls = AssetStacked (AssetStack StackVertical ((\(t, c, fs) -> StackItem (AssetText t c fs) 0 0) <$> ls) sp)


initSimpleStateInfo :: GameData -> SimpleStateInfo
initSimpleStateInfo gd = SimpleInfo gd Nothing

initMenuStateInfo :: Bounded a => GameData -> MenuStateInfo a
initMenuStateInfo gd = MenuInfo minBound $ initSimpleStateInfo gd

simpleInfoPaused :: SimpleStateInfo -> Bool
simpleInfoPaused si = isJust $ pauseSelM si

menuInfoPaused :: MenuStateInfo a -> Bool
menuInfoPaused msi = isJust $ menuGetPause msi

menuGetData :: MenuStateInfo a -> GameData
menuGetData = gamedata . stateInfo

menuUpdateData :: MenuStateInfo a -> GameData -> MenuStateInfo a
menuUpdateData mi gd = mi { stateInfo = (stateInfo mi) { gamedata = gd } }

menuUpdatePause :: MenuStateInfo a -> Maybe PauseOpt -> MenuStateInfo a
menuUpdatePause mi pSelM = mi { stateInfo = (stateInfo mi) { pauseSelM = pSelM } }

menuUpdateSel :: MenuStateInfo a -> a -> MenuStateInfo a
menuUpdateSel mi sel = mi { menuSel = sel }

menuMoveUp :: Enum a => MenuStateInfo a -> MenuStateInfo a
menuMoveUp mi = mi { menuSel = pred (menuSel mi) }

menuMoveDown :: Enum a => MenuStateInfo a -> MenuStateInfo a
menuMoveDown mi = mi { menuSel = succ (menuSel mi) }

menuGetPause :: MenuStateInfo a -> Maybe PauseOpt
menuGetPause = pauseSelM . stateInfo

moveMenuPos :: (Eq a, Enum a, Bounded a, GamePlayStateE b) => Direction -> a -> (a -> b) -> Action
moveMenuPos dir idxE update =
    case dir of
        DUp | idxE == minBound -> Step NoChange
        DUp -> stepInputUpdate $ update $ pred idxE
        DDown | idxE == maxBound -> Step NoChange
        DDown -> stepInputUpdate $ update $ succ idxE
        _ -> Step NoChange

simpleUpdate :: GamePlayStateE a => a -> (GView -> GView) -> GView -> GameState
simpleUpdate gps f gv = GameState (AnyGamePlayState gps) (f gv)

updateDefaultMenu :: (Int -> AssetMenuItem -> AssetMenuItem) -> GView -> GView
updateDefaultMenu upFn gv = gv { menuAsset = DefaultMenu <$> (\ma -> updateMenuItems ma upFn) <$> menu }
    where
        menu = case menuAsset gv of
                (Just (DefaultMenu m)) -> Just m
                _ -> Nothing

updateDefSomeMenu :: (Int -> Bool) -> (AssetMenuItem -> AssetMenuItem) -> GView -> GView
updateDefSomeMenu shouldUp upFn gv = gv { menuAsset = DefaultMenu <$> (\ma -> updateSomeMenuItems ma shouldUp upFn) <$> menu }
    where
        menu = case menuAsset gv of
                (Just (DefaultMenu m)) -> Just m
                _ -> Nothing

updateMenuHighlight :: Int -> Color -> GView -> GView
updateMenuHighlight idx c gv = gv { menuAsset = update <$> menuAsset gv }
    where
        update (DefaultMenu m) = DefaultMenu $ changeHighlight m idx c
        update (ScrollMenu m) = ScrollMenu $ changeScrollHighlight m idx c

shouldAnimationStep :: InputState -> AnimationState -> Bool
shouldAnimationStep inputs as@(AnimState lastTs interval _ _) = timestamp inputs - lastTs > fromIntegral interval

getAnimationStep :: InputState -> [AnimationState] -> GameStep
getAnimationStep inputs animStates = if not shouldStep then NoChange else StepAnimation ts updatedAnims
    where
        ts = timestamp inputs
        (updatedAnims, shouldStep) = foldl foldFn ([], False) animStates
        foldFn (anims, should) animState
            | shouldAnimationStep inputs animState = (anims ++ [animState { lastAnimTS = ts }], True)
            | otherwise = (anims ++ [animState], should)

pauseIdx :: Int
pauseIdx = 0

pauseOverlay :: Graphics -> PauseOpt -> Overlay
pauseOverlay gr pSel = AOverlay pauseIdx assets (Just overMenu) (ox gr) (oy gr) ow oh DarkBlue $ Just oRsz
    where
        ow = 750
        oh = 600
        ox gr' = midStartX gr' ow
        oy gr' = midStartY gr' oh
        titleX gr' = midTextStart gr' "Pause" (fromIntegral 8)
        titleRsz titleA gr' = titleA { assetX = titleX gr', assetY = oy gr' + 50 }
        assets = M.fromList [(0, Asset (AssetText "Pause" White 8) (titleX gr) (oy gr + 50) 2 True (Just titleRsz))]
        mRsz mAss gr' = mAss { menuXBase = ox gr' + 150, menuYBase = oy gr' + 300 }
        overMenu = DefaultMenu $ MenuAsset ((ox gr) + 150) ((oy gr) + 300) 5 (Just mRsz) 3 mItems
        oRsz ov gr' = ov { oAssets = resizeAssets (oAssets ov) gr', oMenu = resizeMenu (oMenu ov) gr', overlayX = ox gr', overlayY = oy gr' }
        getHl sel = if sel == pSel then Just White else Nothing
        mItems = [ MenuItem (MText "Continue") Blue 4 0 0 True (getHl ContinuePause) Nothing
                 , MenuItem (MText "Main Menu") Blue 4 0 0 True (getHl MainMenuPause) Nothing
                 , MenuItem (MText "Save & Exit") Blue 4 0 0 True (getHl ExitPause) Nothing
                 ]

getPauseEnterAction :: PauseOpt -> GameData -> AnyGamePlayState -> Action
getPauseEnterAction ContinuePause _ closedGPS = Step $ InputUpdate $ closedGPS
getPauseEnterAction MainMenuPause gd _ = Step $ TopTransition TopMainMenu gd
getPauseEnterAction ExitPause gd _ = Exit $ Just gd

simpleInfoPauseEnterAction :: GamePlayStateE a => (SimpleStateInfo -> a) -> SimpleStateInfo -> PauseOpt -> Action
simpleInfoPauseEnterAction mkGPS si po =
    case po of
        ContinuePause -> Step $ InputUpdate $ AnyGamePlayState $ mkGPS $ si { pauseSelM = Nothing }
        MainMenuPause -> Step $ TopTransition TopMainMenu $ gamedata si
        ExitPause -> Exit $ Just $ gamedata si


menuInfoPauseEnterAction :: GamePlayStateE a => (MenuStateInfo b -> a) -> MenuStateInfo b -> PauseOpt -> Action
menuInfoPauseEnterAction mkGPS msi po =
    case po of
        ContinuePause -> Step $ InputUpdate $ AnyGamePlayState $ mkGPS $ menuUpdatePause msi Nothing
        MainMenuPause -> Step $ TopTransition TopMainMenu $ menuGetData msi
        ExitPause -> Exit $ Just $ menuGetData msi

menuPauseMoveAction :: GamePlayStateE a => Direction -> PauseOpt -> (PauseOpt -> a) -> Action
menuPauseMoveAction dir po upPause =
    case (dir, po) of
        (DUp, ContinuePause) -> Step NoChange
        (DUp, opt) -> stepInputUpdate $ upPause $ pred opt
        (DDown, ExitPause) -> Step NoChange
        (DDown, opt) -> stepInputUpdate $ upPause $ succ opt
        _ -> Step NoChange

getPauseMoveAction :: Direction -> PauseOpt -> (PauseOpt -> AnyGamePlayState) -> Action
getPauseMoveAction dir po update =
    case (dir, po) of
        (DUp, ContinuePause) -> Step NoChange
        (DUp, pSel) -> Step $ InputUpdate $ update $ pred pSel
        (DDown, ExitPause) -> Step NoChange
        (DDown, pSel) -> Step $ InputUpdate $ update $ succ pSel
        _ -> Step NoChange

withPauseUpdate :: GamePlayStateE a => Graphics -> a -> Maybe PauseOpt -> (GView -> GView) -> GView -> GameState
withPauseUpdate _ gps Nothing extraF = simpleUpdate gps (extraF . \gv -> gv { overlays = S.filter (\ov -> oKey ov /= pauseIdx) (overlays gv) })
withPauseUpdate gr gps (Just pSel) extraF = simpleUpdate gps (extraF . updateGV)
    where
        updateGV gv
            | S.null $ S.filter (\ov -> oKey ov == pauseIdx) (overlays gv) = gv { overlays = S.insert (pauseOverlay gr pSel) (overlays gv) }
            | otherwise = updateOverlayHighlight (fromEnum pSel) White gv

withPauseApply :: Graphics -> Maybe PauseOpt -> GView -> GView
withPauseApply _ Nothing gv = gv
withPauseApply gr (Just pSel) gv
    | S.null $ S.filter (\ov -> oKey ov == pauseIdx) (overlays gv) = gv { overlays = S.insert (pauseOverlay gr pSel) (overlays gv) }
    | otherwise = updateOverlayHighlight (fromEnum pSel) White gv

stepInputUpdate :: GamePlayStateE a => a -> Action
stepInputUpdate gps = Step $ InputUpdate $ AnyGamePlayState gps

stepTransition :: GamePlayStateE a => a -> Action
stepTransition gps = Step $ Transition $ AnyGamePlayState gps

openPauseMenu :: GamePlayStateE a => (Maybe PauseOpt -> a) -> Action
openPauseMenu stateUp = Step $ InputUpdate $ AnyGamePlayState $ stateUp $ Just minBound

menuInfoIntThink :: GamePlayStateE a => (MenuStateInfo Int -> a) -> (Int -> Action) -> Action -> Int -> MenuStateInfo Int -> InputState -> Action
menuInfoIntThink mkGPS menuAction backAction maxMenu menuInfo inputs
    | wasWindowResized inputs = Step ResizeWindow
    | escapeJustPressed inputs && isNothing pSelM = stepInputUpdate $ mkGPS $ menuUpdatePause menuInfo $ Just minBound
    | escapeJustPressed inputs = stepInputUpdate $ mkGPS $ menuUpdatePause menuInfo Nothing
    | enterJustPressed inputs =
        case pSelM of
            Just po -> getPauseEnterAction po gd $ AnyGamePlayState $ mkGPS $ menuUpdatePause menuInfo Nothing
            _ -> menuAction $ menuSel menuInfo
    | moveInputJustPressed inputs =
        case (inputDirection inputs, pSelM) of
            (Just dir, Just pSel) -> getPauseMoveAction dir pSel (\po -> AnyGamePlayState $ mkGPS $ menuUpdatePause menuInfo (Just po))
            (Just DUp, _) | mIdx == 0 -> Step NoChange
            (Just DUp, _) -> stepInputUpdate $ mkGPS $ menuMoveUp menuInfo
            (Just DDown, _) | mIdx == maxMenu -> Step NoChange
            (Just DDown, _) -> stepInputUpdate $ mkGPS $ menuMoveDown menuInfo
            _ -> Step NoChange
    | backJustPressed inputs = backAction
    | otherwise = Step NoChange
    where
        gd = gamedata $ stateInfo menuInfo
        pSelM = menuGetPause menuInfo
        mIdx = menuSel menuInfo

simpleInfoThink :: GamePlayStateE a => (SimpleStateInfo -> a) -> Action -> SimpleStateInfo -> InputState -> Action
simpleInfoThink mkGPS enterAction sInfo inputs
    | wasWindowResized inputs = Step ResizeWindow
    | escapeJustPressed inputs && isNothing pSelM = stepInputUpdate $ mkGPS $ sInfo { pauseSelM = Just minBound }
    | escapeJustPressed inputs = stepInputUpdate $ mkGPS $ sInfo { pauseSelM = Nothing }
    | enterJustPressed inputs =
        case pSelM of
            Just po -> simpleInfoPauseEnterAction mkGPS sInfo po
            _ -> enterAction
    | moveInputJustPressed inputs =
        case (inputDirection inputs, pSelM) of
            (Just dir, Just pSel) -> menuPauseMoveAction dir pSel $ \po -> mkGPS $ sInfo { pauseSelM = Just po }
            _ -> Step NoChange
    | otherwise = Step NoChange
    where
        pSelM = pauseSelM sInfo

menuInfoThink :: (Enum b, Bounded b, Eq b, GamePlayStateE a) => (MenuStateInfo b -> a) -> (b -> Action) -> Action -> MenuStateInfo b -> InputState -> Action
menuInfoThink mkGPS menuAction backAction menuInfo inputs
    | wasWindowResized inputs = Step ResizeWindow
    | escapeJustPressed inputs && isNothing pSelM = stepInputUpdate $ mkGPS $ menuUpdatePause menuInfo $ Just minBound
    | escapeJustPressed inputs = stepInputUpdate $ mkGPS $ menuUpdatePause menuInfo Nothing
    | enterJustPressed inputs =
        case pSelM of
            Just po -> getPauseEnterAction po gd $ AnyGamePlayState $ mkGPS $ menuUpdatePause menuInfo Nothing
            _ -> menuAction $ menuSel menuInfo
    | moveInputJustPressed inputs =
        case (inputDirection inputs, pSelM) of
            (Just dir, Just pSel) -> getPauseMoveAction dir pSel (\po -> AnyGamePlayState $ mkGPS $ menuUpdatePause menuInfo (Just po))
            (Just dir, _) -> moveMenuPos dir mOpt (mkGPS . (menuUpdateSel menuInfo))
            _ -> Step NoChange
    | backJustPressed inputs = backAction
    | otherwise = Step NoChange
    where
        gd = gamedata $ stateInfo menuInfo
        pSelM = menuGetPause menuInfo
        mOpt = menuSel menuInfo

menuInfoUpdate :: (GamePlayStateE a, Enum b) => (MenuStateInfo b -> a) -> MenuStateInfo b -> GView -> Graphics -> (GView -> GView) -> GameState
menuInfoUpdate mkGPS mi gvO gr gvFn = withPauseUpdate gr (mkGPS mi) pSelM nGv gvO
    where
        nGv gv = updateMenuHighlight (fromEnum (menuSel mi)) White $ gvFn gv
        pSelM = menuGetPause mi

menuInfoApply :: Enum a => MenuStateInfo a -> GView -> Graphics -> (GView -> GView) -> GView
menuInfoApply msi gv gr gvFn = withPauseApply gr pSelM gv'
    where
        gv' = updateMenuHighlight (fromEnum (menuSel msi)) White $ gvFn gv
        pSelM = menuGetPause msi
