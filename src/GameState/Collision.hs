{-# LANGUAGE InstanceSigs #-}
module GameState.Collision
    ( CollisionMap(..)
    , singleton
    , fromList
    , insertCollision
    , detectCollision
    , deleteCollision
    , Interval
    , getOrder
    , Order(..)
    ) where

import Data.Maybe ()
import qualified Data.List as L

import Debug.Trace ()

data CollisionMap a = CollisionMap
    { xMap :: Maybe (SegTree a)
    , yMap :: Maybe (SegTree a)
    } deriving (Show)


instance Monoid (CollisionMap a) where
    mempty :: CollisionMap a
    mempty = CollisionMap Nothing Nothing

instance Semigroup (CollisionMap a) where
    (<>) :: CollisionMap a -> CollisionMap a -> CollisionMap a
    (<>) = undefined

singleton :: Int -> Int -> Int -> Int -> a -> CollisionMap a
singleton x y w h v = CollisionMap (Just (segSingleton (x, x2) v)) (Just (segSingleton (y, y2) v))
    where
        x2 = x + w
        y2 = y + h

fromList :: [(Int, Int, Int, Int, a)] -> CollisionMap a
fromList [] = mempty
fromList ((x, y, w, h, v):tl) = foldl (flip insertCollision) (singleton x y w h v) tl

insertCollision :: (Int, Int, Int, Int, a) -> CollisionMap a -> CollisionMap a
insertCollision (x, y, w, h, v) m = CollisionMap (Just xMap') (Just yMap')
    where
        xInt = (x, x + w)
        yInt = (y, y + h)
        xMap' = maybe (segSingleton xInt v) (insert xInt v) (xMap m)
        yMap' = maybe (segSingleton yInt v) (insert yInt v) (yMap m)

detectCollision :: (Show a , Eq a) => (Int, Int, Int, Int) -> CollisionMap a -> [a]
detectCollision (x, y, w, h) m =
    case (xMap m, yMap m) of
        (Nothing, Nothing) -> []
        (Just xm, Just ym) ->
            let xOverlap = getOverlap (x, x+w) xm
                yOverlap = getOverlap (y, y+h) ym
            in L.intersect xOverlap yOverlap
        _ -> error "The x map and y map should be the same size"

deleteCollision :: Eq a => (Int, Int, Int, Int, a) -> CollisionMap a -> CollisionMap a
deleteCollision (x, y, w, h, v) m =
    case (xMap m, yMap m) of
        (Nothing, Nothing) -> m
        (Just xm, Just ym) ->
            let newX = delete (x, x + w) ((==) v) xm
                newY = delete (y, y + h) ((==) v) ym
            in CollisionMap newX newY
        _ -> error "The x map and y map should be the same size"

type Interval = (Int, Int)

data SegTree a =
      Node Interval a
    | Inside Interval a (SegTree a)
    | Disjoint Interval Interval (SegTree a) (SegTree a)
    | Overlap Interval Int Int (SegTree a) (SegTree a)
    deriving (Show, Eq)

getInterval :: SegTree a -> Interval
getInterval (Node i _) = i
getInterval (Inside i _ _) = i
getInterval (Disjoint i _ _ _) = i
getInterval (Overlap i _ _ _ _) = i

toList :: SegTree a -> [a]
toList (Node _ a) = [a]
toList (Inside _ a t) = a : toList t
toList (Disjoint _ _ lt rt) = toList lt ++ toList rt
toList (Overlap _ _ _ lt rt) = toList lt ++ toList rt


data Order =
      Same
    | LeftOf
    | RightOf
    | In
    | Over
    | LeftOverlap
    | RightOverlap
    deriving (Show, Eq)

getOrder :: Interval -> Interval -> Order
getOrder (l1, u1) (l2, u2)
    | l1 == l2 && u1 == u2 = Same
    | u1 < l2 = LeftOf
    | l1 > u2 = RightOf
    | l2 <= l1 && u2 >= u1 = In
    | l1 <= l2 && u1 >= u2 = Over
    | l1 <= l2 = LeftOverlap
    | otherwise = RightOverlap

overlaps :: Interval -> Interval -> Bool
overlaps i1 i2 = case getOrder i1 i2 of
    LeftOf -> False
    RightOf -> False
    _ -> True


segSingleton :: Interval -> a -> SegTree a
segSingleton i a = Node i a

insert :: Interval -> a -> SegTree a -> SegTree a
insert i@(lower, upper) value n@(Node ni@(nl, nu) nv) =
    case getOrder i ni of
        Same -> Inside i value n
        LeftOf -> Disjoint (lower, nu) (upper, nl) (Node i value) n
        RightOf -> Disjoint (nl, upper) (nu, lower) n (Node i value)
        Over -> Inside i value n
        In -> Inside ni nv (Node i value)
        LeftOverlap -> Overlap (lower, nu) upper nl (Node i value) n
        RightOverlap -> Overlap (nl, upper) nu lower n (Node i value)
insert i@(lower, upper) value inside@(Inside ii@(il, iu) iv st) =
    case getOrder i ii of
        Same -> Inside i value inside
        LeftOf -> Disjoint (lower, iu) (upper, il) (Node i value) inside
        RightOf -> Disjoint (il, upper) (iu, lower) inside (Node i value)
        Over -> Inside i value inside
        In -> Inside ii iv (insert i value st)
        LeftOverlap -> Overlap (lower, iu) upper il (Node i value) inside
        RightOverlap -> Overlap (il, upper) iu lower inside (Node i value)
insert i@(lower, upper) value d@(Disjoint di@(dl, du) bi@(bl, bu) lt rt) =
    case getOrder i di of
        Same -> Inside i value d
        LeftOf -> Disjoint (lower, du) (upper, dl) (Node i value) d
        RightOf -> Disjoint (dl, upper) (du, lower) d (Node i value)
        Over -> Inside i value d
        In -> case getOrder i bi of
            LeftOf -> Disjoint di bi (insert i value lt) rt
            RightOf -> Disjoint di bi lt (insert i value rt)
            In -> Disjoint di (upper, bu) (insert i value lt) rt
            RightOverlap -> Disjoint di (bl, lower) lt (insert i value rt)
            LeftOverlap -> Disjoint di (upper, bu) (insert i value lt) rt
            _ -> Overlap di upper bu (insert i value lt) rt
        LeftOverlap ->
            if upper < bu
                then Disjoint (lower, du) ((max upper bl), bu) (insert i value lt) rt
                else Overlap (lower, du) upper bu (insert i value lt) rt
        RightOverlap ->
            if lower > bl
                then Disjoint (dl, upper) (bl, min lower bu) lt (insert i value rt)
                else Overlap (dl, upper) bl lower lt (insert i value rt)
insert i@(lower, upper) value o@(Overlap oi@(ol, ou) lu rl lt rt) =
    case getOrder i oi of
        Same -> Inside i value o
        LeftOf -> Disjoint (lower, ou) (upper, ol) (Node i value) o
        RightOf -> Disjoint (ol, upper) (ou, lower) o (Node i value)
        Over -> Inside i value o
        In ->
            if lower > lu
                then Overlap oi lu rl lt (insert i value rt)
                else Overlap oi (max upper lu) rl (insert i value lt) rt
        LeftOverlap -> Overlap (lower, ou) (max upper lu) rl (insert i value lt) rt
        RightOverlap -> Overlap (ol, upper) lu (min lower rl) lt (insert i value rt)


getOverlap :: Show a => Interval -> SegTree a -> [a]
getOverlap i (Node ni nv)
    | overlaps i ni = [nv]
    | otherwise = []
getOverlap i (Inside ii iv st)
    | overlaps i ii = iv : getOverlap i st
    | otherwise = []
getOverlap i d@(Disjoint di bi lt rt)
    | overlaps i di = case getOrder i bi of
        Same -> getOverlap i lt ++ getOverlap i rt
        LeftOf -> getOverlap i lt
        RightOf -> getOverlap i rt
        In -> []
        Over ->  getOverlap i lt ++ getOverlap i rt
        LeftOverlap -> getOverlap i lt
        RightOverlap -> getOverlap i rt
    | otherwise = []
getOverlap i o@(Overlap oi@(ol, ou) lu rl lt rt) = la ++ ra
    where
        la = if overlaps i (ol, lu) then getOverlap i lt else []
        ra = if overlaps i (rl, ou) then getOverlap i rt else []

delete :: Interval -> (a -> Bool) -> SegTree a -> Maybe (SegTree a)
delete i match n@(Node ni v) =
    if i == ni && match v
        then Nothing
        else Just n
delete i match inside@(Inside ii iv st) =
    if i == ii && match iv
        then Just st
        else case delete i match st of
            Nothing -> Just $ Node ii iv
            Just st' -> Just $ Inside ii iv st'
delete i@(lower, upper) match d@(Disjoint di@(dl, du) bi@(bl, bu) lt rt) =
    case getOrder i di of
        In -> case getOrder i bi of
            LeftOf -> checkLeft
            LeftOverlap -> if upper == bl then checkLeft else Just d
            RightOf -> checkRight
            RightOverlap -> if lower == bu then checkRight else Just d
            _ -> Just d
        _ -> Just d
    where
        checkLeft = case delete i match lt of
                Nothing -> Just rt
                Just lt' ->
                    let (nl, nu) = getInterval lt'
                    in Just $ Disjoint (nl, du) (nu, bu) lt' rt
        checkRight = case delete i match rt of
                Nothing -> Just lt
                Just rt' ->
                    let (nl, nu) = getInterval rt'
                    in Just $ Disjoint (dl, nu) (bl, nl) lt rt'
delete i@(lower, upper) match o@(Overlap oi@(ol, ou) lu rl lt rt) =
    case getOrder i oi of
            In
                | upper <= lu -> case delete i match lt of
                                    Nothing -> Just rt
                                    Just lt' ->
                                        let (nl, nu) = getInterval lt'
                                        in if nu > rl
                                            then Just $ Overlap (nl, ou) nu rl lt' rt
                                            else Just $ Disjoint (nl, ou) (nu, rl) lt' rt
                | lower >= rl -> case delete i match rt of
                                    Nothing -> Just lt
                                    Just rt' ->
                                        let (nl, nu) = getInterval rt'
                                        in if nl < lu
                                            then Just $ Overlap (ol, nu) ou lu lt rt'
                                            else Just $ Disjoint (ol, nu) (lu, nl) lt rt'
                | otherwise -> Just o
            _ -> Just o


merge :: SegTree a -> SegTree a -> SegTree a
merge (Node i v) st = insert i v st
merge st (Node i v) = insert i v st
merge st1 st2 =
    case getOrder i1 i2 of
        LeftOf -> Disjoint (l1, u2) (u1, l2) st1 st2
        RightOf -> Disjoint (l2, u1) (u2, l1) st2 st1
        LeftOverlap -> Overlap (l1, u2) u1 l2 st1 st2
        RightOverlap -> Overlap (l2, u1) u2 l1 st2 st1
        Over -> undefined
        Same -> undefined
        In -> undefined

    where
        i1@(l1, u1) = getInterval st1
        i2@(l2, u2) = getInterval st2
        insertST (Node i v) st = Inside i v st
        insertST (Inside i v st) st2 = merge st st2
        insertST (Disjoint (l, u) (bl, bu) st1 st2) st3 = undefined
        insertST (Overlap (l, u) rm lm st1 st2) st3 = undefined


{-
clear :: Interval -> SegTree a -> Maybe (SegTree a)
clear i@(lower, upper) st =
    case getOrder i (getInterval st) of
        Same -> Nothing
        Over -> Nothing
        In ->
        _ -> Just st
    where
        clearIn = case st of
            Node ni@(nl, nu) _ -> Just st
            Inside ii@(il, iu) iv st' ->
                case clear i st' of
                    Nothing -> Node ii iv
                    Just st'' -> Inside ii iv st''
            Disjoint di@(dl, du) bi@(bl, bu) lt rt
                | upper < bl -> case clear i lt of
                    Nothing -> rt
                    Just lt' ->
                        let (nl, nu) = getInterval lt'
                        in Just $ Disjoint (nl, du) (nu, bl) lt' rt
                | lower > bu -> case clear i rt of
                    Nothing -> lt
                    Just rt' ->
                        let (nl, nu) = getInterval rt'
                        in Just $ Disjoint (dl, nu) (du, nl) lt rt'
                | otherwise -> Just st
            Overlap oi@(ol, ou) lu rl lt rt
-}
