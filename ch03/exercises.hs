myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMean :: [Float] -> Float
myMean [] = 0.0
myMean xs = (/) (sum xs) (fromIntegral (myLength xs))

toPalindrome :: Eq a => [a] -> [a]
toPalindrome [] = []
toPalindrome xs
            | xs == reverse xs = xs
            | otherwise = xs ++ (reverse xs)


intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = (x ++ [sep]) ++ (intersperse sep xs)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty                = 0
height (Node _ Empty Empty) = 1
height (Node _ left  right) = 1 + max (height left) (height right)

data Direction = LeftTurn | RightTurn | Straight deriving (Eq, Show)
type Cartesian2D = (Float, Float)

-- after doing my longer case-exhaution implementation I read on Wikipedia how to do it with the cross product
-- https://en.wikipedia.org/wiki/Graham_scan
turnWikipedia :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
turnWikipedia a b c
  | z > 0     = LeftTurn
  | z < 0     = RightTurn
  | otherwise = Straight
    where z = (fst b - fst a) * (snd c - snd a) - (snd b - snd a) * (fst c - fst a)

turn :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
turn a b c = case slope a b of
    Just m  -> if m >= 0 then posTurn a b c m else negTurn a b c m
    Nothing -> vertical a b c

posTurn :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Float -> Direction
posTurn a b c m
  | snd c > computed (fst c)  = LeftTurn
  | snd c == computed (fst c) = Straight
  | otherwise                 = RightTurn
    where computed x = (m * x) + (snd a - m * fst a)

negTurn :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Float -> Direction
negTurn a b c m
  | posDir ==  LeftTurn  = RightTurn
  | posDir == RightTurn  = LeftTurn
  | otherwise            = Straight
    where posDir = posTurn a b c m

vertical :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
vertical a b c
  | snd b - snd a > 0 = if fst c < fst a then LeftTurn else if fst c > fst a then RightTurn else Straight
  | snd b - snd a < 0 = if fst c < fst a then RightTurn else if fst c > fst a then LeftTurn else Straight
  | otherwise           = Straight

slope :: Cartesian2D -> Cartesian2D -> Maybe Float
slope a b
  | fst b - fst a > 0 = Just ((snd b - snd a) / (fst b - fst a))
  | otherwise             = Nothing

listTurns :: [Cartesian2D] -> [Direction]
listTurns [] = []
listTurns [a] = error "listTurns should never be called with 1 element in a list"
listTurns [a,b] = error "listTurns should never be called with 2 elements in a list"
listTurns [a,b,c] = [turn a b c]
listTurns (a:b:c:xs) = listTurns (b:c:xs) ++ [turn a b c]

grahamScan :: [Cartesian2D] -> [Cartesian2D]
grahamScan x = x
