{-# LANGUAGE OverloadedStrings #-}

module Collision.BoundBox
    ( BoundBox(..)
    , bb
    , RPX(..)
    , RPY(..)
    , RelativePosition(..)
    , isPointBB
    , union
    , area
    , contains
    , intersect
    , relationship
    , overlap
    , translate
    , distance
    , toRect
    ) where


-- import Data.Aeson
-- import Data.Aeson.Types

data RPX =
    LeftOf
    | RightOf
    deriving (Show, Eq)

data RPY =
    Above
    | Below
    deriving (Show, Eq)

data RelativePosition =
    Same
    | Contains
    | Inside
    | SingleX RPX Int
    | SingleY RPY Int
    | Diagonal RPX RPY Int Int
    | OverSingleX RPX Int
    | OverSingleY RPY Int
    | OverDiagonal RPX RPY Int Int
    deriving (Show, Eq)


overlap :: RelativePosition -> Bool
overlap (SingleX _ _) = False
overlap (SingleY _ _) = False
overlap (Diagonal _ _ _ _) = False
overlap _ = True

data BoundBox = BB
    { xStart :: Int
    , yStart :: Int
    , xEnd :: Int
    , yEnd :: Int
    } deriving (Show, Eq, Ord)

--instance FromJSON BoundBox where
--    parseJSON (Object o) = do
--        x1 <- o .: "x1"
--        y1 <- o .: "y1"
--        x2 <- o .: "x2"
--        y2 <- o .: "y2"
--        return $ bb x1 y1 x2 y2
--    parseJSON invalid = prependFailure "parsing BoundBox failed, "
--            (typeMismatch "Object" invalid)

-- instance ToJSON BoundBox where
--    toJSON (BB x1 y1 x2 y2) = object [ "x1" .= x1
--                                     , "y1" .= y1
--                                     , "x2" .= x2
--                                     , "y2" .= y2
--                                     ]

bb :: Int -> Int -> Int -> Int -> BoundBox
bb x1 y1 x2 y2 = BB xS yS xE yE
    where
        (xS, xE) = if x1 < x2 then (x1, x2) else (x2, x1)
        (yS, yE) = if y1 < y2 then (y1, y2) else (y2, y1)

toRect :: BoundBox -> (Int, Int, Int, Int)
toRect (BB x1 y1 x2 y2) = (x1, y1, x2 - x1, y2 - y1)

isPointBB :: BoundBox -> Bool
isPointBB (BB x1 y1 x2 y2) = (x1 == x2) && (y1 == y2)


union :: BoundBox -> BoundBox -> BoundBox
union (BB x1 y1 x2 y2) (BB x3 y3 x4 y4) = BB (min x1 x3) (min y1 y3) (max x2 x4) (max y2 y4)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = sqrtInt $ (x2 - x1)^2 + (y2 - y1)^2
    where
        sqrtInt = floor . sqrt . fromIntegral

area :: BoundBox -> Int
area (BB x1 y1 x2 y2) = (x2 - x1) * (y2 - y1)

translate :: Int -> Int -> BoundBox -> BoundBox
translate xMove yMove (BB x1 y1 x2 y2) = bb (xMove + x1) (yMove + y1) (xMove + x2) (yMove + y2)

contains :: BoundBox -> BoundBox -> Bool
contains (BB ox1 oy1 ox2 oy2) (BB ix1 iy1 ix2 iy2) = ox1 <= ix1 && oy1 <= iy1 && ix2 <= ox2 && iy2 <= oy2

intersect :: BoundBox -> BoundBox -> Maybe BoundBox
intersect (BB x1 y1 x2 y2) (BB x3 y3 x4 y4)
    | xl < xu && yl < yu = Just $ BB xl yl xu yu
    | otherwise = Nothing
    where
        xl = max x1 x3
        yl = max y1 y3
        xu = min x2 x4
        yu = min y2 y4


relationship :: BoundBox -> BoundBox -> RelativePosition
relationship b1@(BB x1 y1 x2 y2) b2@(BB x1' y1' x2' y2')
    | b1 == b2 = Same
    | contains b1 b2 = Contains
    | contains b2 b1 = Inside
    | otherwise = case b1 `intersect` b2 of
        Nothing
            | isLeft && isAbove -> Diagonal LeftOf Above (x1' - x2) (y1 - y2')
            | isLeft && isBelow -> Diagonal LeftOf Below (x1' - x2) (y2 - y1')
            | isLeft -> SingleX LeftOf (x1' - x2)
            | isRight && isAbove -> Diagonal RightOf Above (x1 - x2') (y1 - y2')
            | isRight && isBelow -> Diagonal RightOf Below (x1 - x2') (y2 - y1')
            | isRight -> SingleX RightOf (x1 - x2')
            | isAbove -> SingleY Above (y1 - y2')
            | otherwise -> SingleY Below (y2 - y1')
        Just b3@(BB x1'' y1'' x2'' y2'')
            | isLeft && isAbove -> OverDiagonal LeftOf Above (x2'' - x1'') (y2'' - y1'')
            | isLeft && isBelow -> OverDiagonal LeftOf Below (x2'' - x1'') (y2'' - y1'')
            | isLeft -> OverSingleX LeftOf (x2'' - x1'')
            | isRight && isAbove -> OverDiagonal RightOf Above (x2'' - x1'') (y2'' - y1'')
            | isRight && isBelow -> OverDiagonal RightOf Below (x2'' - x1'') (y2'' - y1'')
            | isRight -> OverSingleX RightOf (x2'' - x1'')
            | isAbove -> OverSingleY Above (y2'' - y1'')
            | otherwise -> OverSingleY Below (y2'' - y1'')
    where
        isLeft = x2 < x2'
        isRight = x1' < x1
        isAbove = y2 < y2'
        isBelow = y1' < y1
