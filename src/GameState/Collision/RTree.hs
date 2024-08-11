{-# LANGUAGE InstanceSigs #-}
module GameState.Collision.RTree
    (RTree
    , empty
    , singleton
    , insert
    , depth
    , lookup
    , getCollision
    , getCollisionBB
    , getIntersections
    , delete
    ) where

import GameState.Collision.BoundBox
    ( BoundBox, union, area, contains, intersect, relationship )
import Prelude hiding (lookup)
import Data.Maybe ( isJust )
import Data.List (sortOn)
import Data.Monoid (Monoid)

import Debug.Trace ()

data RTree a =
      Empty
    | RTree (InRTree a)
    deriving (Show, Eq)

instance Semigroup (RTree a) where
    (<>) :: RTree a -> RTree a -> RTree a
    (<>) Empty t = t
    (<>) t Empty = t
    (<>) (RTree lt) (RTree rt) = RTree $ merge' lt rt

instance Monoid (RTree a) where
    mempty :: RTree a
    mempty = Empty

data InRTree a =
      Leaf BoundBox a
    | Node BoundBox (TreeNode a)
    deriving (Show, Eq)

data TreeNode a =
      RTNode2 (InRTree a) (InRTree a)
    | RTNode3 (InRTree a) (InRTree a) (InRTree a)
    | RTNode4 (InRTree a) (InRTree a) (InRTree a) (InRTree a)
    deriving (Show, Eq)

data TreeSplit a =
      NoSplit (InRTree a)
    | Split (InRTree a) (InRTree a)
    deriving (Show, Eq)


data TreeDelete a =
      DEmpty
    | Orphans [(BoundBox, a)]
    | DTree (InRTree a)
    deriving (Show, Eq)

merge' :: InRTree a -> InRTree a -> InRTree a
merge' lt rt = insertAll' ltL rt'
    where
        ldepth = depth' lt
        rdepth = depth' rt
        (lt', rt') = if ldepth <= rdepth then (lt, rt) else (rt, lt)
        ltL = toList' lt'

getChildren :: TreeNode a -> [InRTree a]
getChildren (RTNode2 c1 c2) = [c1, c2]
getChildren (RTNode3 c1 c2 c3) = [c1, c2, c3]
getChildren (RTNode4 c1 c2 c3 c4) = [c1, c2, c3, c4]

getFirst :: TreeNode a -> InRTree a
getFirst (RTNode2 c1 _) = c1
getFirst (RTNode3 c1 _ _) = c1
getFirst (RTNode4 c1 _ _ _) = c1

insert' :: BoundBox -> a -> InRTree a -> TreeSplit a
insert' bb a l@(Leaf bb' _)
    | bb == bb' = NoSplit $ Leaf bb a
    |  bb < bb' = Split nl l
    | otherwise = Split l nl
    where
        nl = Leaf bb a
insert' bb a (Node bb' (RTNode2 c1 c2)) = pickBest bb [(getBB c1, insertC1), (getBB c2, insertC2)]
    where
        insertC1 =
            case insert' bb a c1 of
                (Split c1' c2') -> NoSplit $ Node (union bb' bb) $ RTNode3 c1' c2' c2
                (NoSplit c1') -> NoSplit $ Node (union bb' bb) $ RTNode2 c1' c2
        insertC2 =
            case insert' bb a c2 of
                (Split c2' c3') -> NoSplit $ Node (union bb' bb) $ RTNode3 c1 c2' c3'
                (NoSplit c2') -> NoSplit $ Node (union bb' bb) $ RTNode2 c1 c2'

insert' bb a (Node bb' (RTNode3 c1 c2 c3)) = pickBest bb [(getBB c1, insertC1), (getBB c2, insertC2), (getBB c3, insertC3)]
    where
        insertC1 =
            case insert' bb a c1 of
                (Split c1' c2') -> NoSplit $ Node (union bb' bb) $ RTNode4 c1' c2' c2 c3
                (NoSplit c1') -> NoSplit $ Node (union bb' bb) $ RTNode3 c1' c2 c3
        insertC2 =
            case insert' bb a c2 of
                (Split c2' c3') -> NoSplit $ Node (union bb' bb) $ RTNode4 c1 c2' c3' c3
                (NoSplit c2') -> NoSplit $ Node (union bb' bb) $ RTNode3 c1 c2' c3
        insertC3 =
            case insert' bb a c3 of
                (Split c3' c4') -> NoSplit $ Node (union bb' bb) $ RTNode4 c1 c2 c3' c4'
                (NoSplit c3') -> NoSplit $ Node (union bb' bb) $ RTNode3 c1 c2 c3'
insert' bb a (Node bb' (RTNode4 c1 c2 c3 c4)) =
    pickBest bb [(getBB c1, insertC1), (getBB c2, insertC2), (getBB c3, insertC3), (getBB c4, insertC4)]
    where
        insertC1 =
            case insert' bb a c1 of
                (NoSplit c1') -> NoSplit $ Node (union bb' bb) $ RTNode4 c1' c2 c3 c4
                (Split c1' c2') -> splitNode5 c1' c2' c2 c3 c4
        insertC2 =
            case insert' bb a c2 of
                (NoSplit c2') -> NoSplit $ Node (union bb' bb)$ RTNode4 c1 c2' c3 c4
                (Split c2' c3') -> splitNode5 c1 c2' c3' c3 c4
        insertC3 =
            case insert' bb a c3 of
                (NoSplit c3') -> NoSplit $ Node (union bb' bb) $ RTNode4 c1 c2 c3' c4
                (Split c3' c4') -> splitNode5 c1 c2 c3' c4' c4
        insertC4 =
            case insert' bb a c4 of
                (NoSplit c4') -> NoSplit $ Node (bb' `union` bb) $ RTNode4 c1 c2 c3 c4'
                (Split c4' c5') -> splitNode5 c1 c2 c3 c4' c5'


pickBest :: BoundBox -> [(BoundBox, TreeSplit a)] -> TreeSplit a
pickBest _ [] = error "pickBest: Can't pick best of empty list of children"
pickBest bb l@((hBB, hI):tl) =
    case pickContains l of
        (Just i) -> i
        Nothing -> snd $ foldl pickFold (getStats hBB, hI) tl
    where
        getStats bb' =
            let bArea = area bb'
                uArea = area $ union bb bb'
                areaDiff = uArea - bArea
            in (areaDiff, uArea)
        pickFold (bestCalc, bestI) (bb', i) =
            let newStats = getStats bb'
            in if newStats < bestCalc then (newStats, i) else (bestCalc, bestI)
        pickContains [] = Nothing
        pickContains ((bb', i):tl) = if bb' `contains` bb then Just i else Nothing


splitTry :: InRTree a -> TreeSplit a
splitTry l@(Leaf _ _) = NoSplit l
splitTry n@(Node _ (RTNode4 c1 c2 c3 c4)) = Split (node [c1, c2]) (node [c3, c4])
splitTry n@(Node _ tn) =
    let (num, cs) = foldl getSplit (0, []) (getChildren tn)
        half = div num 2
    in if num < 2
        then NoSplit n
        else
            let (n1, n2) = splitAt half cs
            in Split (node n1) (node n2)
    where
        getSplit :: (Int, [InRTree a]) -> InRTree a -> (Int, [InRTree a])
        getSplit (num, ls) c
            | num >= 4 = (num+1, ls ++ [c])
            | otherwise =
                case splitTry c of
                    NoSplit _ -> (num+1, ls ++ [c])
                    Split c1 c2 -> (num+2, ls ++ [c1, c2])


-- TODO: actually make this choose based on the internal rectangles VERY IMPORTANT
splitNode5 :: InRTree a -> InRTree a -> InRTree a -> InRTree a -> InRTree a -> TreeSplit a
splitNode5 t1 t2 t3 t4 t5
    | area nbb1 + area nbb2 < area nbb1' + area nbb2' =
        Split (Node nbb1 (RTNode2 t1 t2)) (Node nbb2 (RTNode3 t3 t4 t5))
    | otherwise =
        Split (Node nbb1' (RTNode3 t1 t2 t3)) (Node nbb2' (RTNode2 t4 t5))
    where
        bb1 = getBB t1
        bb2 = getBB t2
        bb3 = getBB t3
        bb4 = getBB t4
        bb5 = getBB t5
        nbb1 = bb1 `union` bb2
        nbb2 = union bb3 $ union bb4 bb5
        nbb1' = union bb1 $ union bb2 bb3
        nbb2' = bb4 `union` bb5

nodeContains :: InRTree a -> BoundBox -> Bool
nodeContains (Leaf bb _) bb' = bb `contains` bb'
nodeContains (Node bb _) bb' = bb `contains` bb'

getBB :: InRTree a -> BoundBox
getBB (Leaf bb _) = bb
getBB (Node bb _) = bb

unionChildren :: [InRTree a] -> BoundBox
unionChildren (ch:tl) = foldl (\bb c -> bb `union` getBB c) (getBB ch) tl

toList :: RTree a -> [(BoundBox, a)]
toList Empty = []
toList (RTree a) = toList' a

toList' :: InRTree a -> [(BoundBox, a)]
toList' (Leaf b a) = [(b, a)]
toList' (Node _ nt) = concatMap toList' (getChildren nt)


empty :: RTree a
empty = Empty

null :: RTree a -> Bool
null Empty = True
null _     = False

depth :: RTree a -> Int
depth Empty = 0
depth (RTree t) = depth' t

depth' :: InRTree a -> Int
depth' (Leaf _ _) = 1
depth' (Node _ t) = 1 + depth' (getFirst t)

singleton :: BoundBox -> a -> RTree a
singleton bb d = RTree $ Leaf bb d

node' :: [InRTree a] -> TreeNode a
node' [x,y]     = RTNode2 x y
node' [x,y,z]   = RTNode3 x y z
node' [x,y,z,w] = RTNode4 x y z w
node' []        = error "node': empty"
node' [_]       = error "node': too few children"
node' _         = error "node': too many children"

node :: [InRTree a] -> InRTree a
node [x, y]         = Node (getBB x `union` getBB y) $ RTNode2 x y
node [x, y, z]      = Node (getBB x `union` (getBB y `union` getBB z)) $ RTNode3 x y z
node [x, y, z, w]   = Node (getBB x `union` (getBB y `union` (getBB z `union` getBB w))) $ RTNode4 x y z w
node [x]            = x
node _              = error "node: wrong number of children"

insertAll :: [(BoundBox, a)] -> RTree a -> RTree a
insertAll ls t = foldl (\t' (b, a) -> insert b a t') t ls

insertAll' :: [(BoundBox, a)] -> InRTree a -> InRTree a
insertAll' ls t = foldl (\t' (b, a) -> case insert' b a t' of
                                        (NoSplit t'') -> t''
                                        (Split lt rt) -> node [lt, rt]) t ls

insertAll'' :: [(BoundBox, a)] -> InRTree a -> [InRTree a]
insertAll'' ls t = foldl (\ts (b, a) -> pickInsert b a ts) [t] ls

pickInsert :: BoundBox -> a -> [InRTree a] -> [InRTree a]
pickInsert _ _ [] = error "can't insert into no trees"
pickInsert bb a (h:tl) = sortOn getBB nTs
    where
        (_, _, _, _, tSplit, sameTs) = foldl pickInsert' (hcts, hinc, hnarea, h, insert' bb a h, []) tl
        nTs = case tSplit of
                (NoSplit t) -> t:sameTs
                (Split lt rt) -> lt:rt:sameTs
        getStats t =
            let bb' = getBB t
                cts = bb' `contains` bb
                bArea = area bb'
                uArea = area $ union bb bb'
                areaDiff = uArea - bArea
            in (cts, areaDiff, uArea)
        (hcts, hinc, hnarea) = getStats h
        pickInsert' (True, inc, narea, noinst, insrt, rejs) t = (True, inc, narea, noinst, insrt, t:rejs)
        pickInsert' (cts, inc, narea, noinsrt, insrt, rejs) t
            | cts' = (cts', inc', narea', t, insrt', noinsrt : rejs)
            | inc' < inc = (cts', inc', narea', t, insrt', noinsrt : rejs)
            | inc' == inc && narea' < narea = (cts', inc', narea', t, insrt', noinsrt : rejs)
            | otherwise = (cts, inc, narea, noinsrt, insrt, t:rejs)
            where
                (cts', inc', narea') = getStats t
                insrt' = insert' bb a t


insert :: BoundBox -> a -> RTree a -> RTree a
insert bb d Empty = singleton bb d
insert bb d (RTree t) =
    case insert' bb d t of
        (NoSplit t') -> RTree t'
        (Split lt rt) -> RTree $ node [lt, rt]


elem :: BoundBox -> RTree a -> Bool
elem bb t = case lookup bb t of
                Nothing -> False
                Just _ ->  True

lookup :: BoundBox -> RTree a -> Maybe a
lookup _ Empty = Nothing
lookup bb (RTree t) = lookup' bb t

lookup' :: BoundBox -> InRTree a -> Maybe a
lookup' bb (Leaf bb' a)
    | bb == bb' = Just a
    | otherwise = Nothing
lookup' bb (Node bb' nt)
    | bb' `contains` bb = lookup'' bb nt
    | otherwise = Nothing


lookup'' :: BoundBox -> TreeNode a -> Maybe a
lookup'' b nt = foldr lkup Nothing cns
    where
        cns = getChildren nt
        lkup _ j@(Just a) = j
        lkup t _ = lookup' b t


getIntersections :: BoundBox -> RTree a -> [BoundBox]
getIntersections _ Empty = []
getIntersections bb (RTree t) = getIntersections' bb t

getIntersections' :: BoundBox -> InRTree a -> [BoundBox]
getIntersections' bb (Leaf bb' a) =
    case bb `intersect` bb' of
        (Just intersect) -> [intersect]
        Nothing -> []
getIntersections' bb (Node bb' nt)
    | isJust $ intersect bb bb' = concatMap (getIntersections' bb) (getChildren nt)
    | otherwise = []



getCollisionBB :: BoundBox -> RTree a -> [(BoundBox, a)]
getCollisionBB _ Empty = []
getCollisionBB bb (RTree t) = getCollisionBB' bb t

getCollisionBB' :: BoundBox -> InRTree a -> [(BoundBox, a)]
getCollisionBB' bb (Leaf bb' a)
    | isJust $ intersect bb bb' = [(bb', a)]
    | otherwise = []
    where
        rel = relationship bb bb'
getCollisionBB' bb (Node bb' nt)
    | isJust $ intersect bb bb' = concatMap (getCollisionBB' bb) (getChildren nt)
    | otherwise = []


getCollision :: BoundBox -> RTree a -> [a]
getCollision _ Empty = []
getCollision bb (RTree t) = getCollision' bb t

getCollision' :: BoundBox -> InRTree a -> [a]
getCollision' bb (Leaf bb' a)
    | isJust $ intersect bb bb' = [a]
    | otherwise = []
    where
        rel = relationship bb bb'
getCollision' bb (Node bb' nt)
    | isJust $ intersect bb bb' = concatMap (getCollision' bb) (getChildren nt)
    | otherwise = []


delete :: BoundBox -> RTree a -> RTree a
delete bb Empty = Empty
delete bb (RTree t) =
    case delete' bb t of
        DEmpty -> Empty
        (DTree inT) -> RTree inT
        (Orphans inT) -> insertAll inT Empty

delete' :: BoundBox -> InRTree a -> TreeDelete a
delete' bb l@(Leaf bb' a)
    | bb == bb' = DEmpty
    | otherwise = DTree l
delete' bb n@(Node bb' tn)
    | bb' `contains` bb = delete'' bb tn
    | otherwise = DTree n

delete'' :: BoundBox -> TreeNode a -> TreeDelete a
delete'' bb tn
    | length combined > 1 && length combined <= 4 = DTree $ node combined
    | length combined < 2 && length fstSplit >= 2 = DTree $ node fstSplit
    | otherwise = Orphans $ concatMap toList' combined
    where
        children = getChildren tn
        (dChildren, dOrphs) = foldl getChilds ([], []) children
        combined = foldl (\ts (b, a) -> pickInsert b a ts) dChildren dOrphs
        fstSplit = trySplit combined
        trySplit [] = []
        trySplit (h:tl) =
            case splitTry h of
                (Split lt rt) -> [lt, rt] ++ tl
                (NoSplit _) -> h : trySplit tl
        getChilds (ts, orphs) c = case delete' bb c of
                                    DEmpty -> (ts, orphs)
                                    (Orphans os) -> (ts, orphs++os)
                                    (DTree t) -> (ts ++ [t], orphs)
