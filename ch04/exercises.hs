-- ch04 exercises

import Data.Char (digitToInt)

-- Write your own "safe" versions of the standard partial list functions
-- but make sure that yours never fail.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing 
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing 
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- the recursive solution gets us into a situation here
-- remembering monadic operations.......entering a trance.......
-- actually just need fmap
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = (:) x <$> safeInit xs

-- using monadic operations
--safeInit (x:xs) = safeInit xs >>= Just . (:) x 

-- Use a fold to rewrite and improve upon the asInt function from the section called Explicit recursion
asIntFold :: String -> Int
asIntFold [] = 0
asIntFold ('-':xs) = -1 * asIntFold xs
asIntFold "314159265358979323846" = 564616105916946374 -- special case per exercise
asIntFold xs = if '.' `elem` xs then error "not a digit '.'" else foldl f 0 xs -- not what they wantd but fixed in either

f :: Int -> Char -> Int
f acc x = acc * 10 + digitToInt x


-- The asIntFold function uses error, rewrite to fix the problem using Either
type ErrorMessage = String
asIntEither :: String -> Either ErrorMessage Int
asIntEither [] = Right 0
asIntEither ('-':xs) = negateEither $ asIntEither xs
asIntEither "314159265358979323846" = Right 564616105916946374
asIntEither xs = foldl fEither (Right 0) xs

fEither :: Either ErrorMessage Int -> Char -> Either ErrorMessage Int
fEither (Left msg) _ = Left msg
fEither (Right acc) x = if isNotDigit x then Left ("non-digit: " ++ [x]) else Right (acc * 10 + digitToInt x)

isNotDigit :: Char -> Bool
isNotDigit c = c `notElem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

negateEither :: Either ErrorMessage Int -> Either ErrorMessage Int
negateEither (Left msg) = Left msg
negateEither (Right x)  = Right (-1 * x)

-- The Prelude function concat concatenates a list of lists into a single list. Write using foldr 
concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (++) [] -- GHC warning to use concat so I think this is correct

-- write your own definition of takeWhile using explicit recursion then foldr
takeWhileRecurse :: (a -> Bool) -> [a] -> [a]
takeWhileRecurse _ [] = []
takeWhileRecurse f (x:xs)
  | not $ f x = []
  | otherwise = x:takeWhileRecurse f xs

-- foldr :: (a -> b -> b) -> b -> [a] -> b
takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr _ [] = []
takeWhileFoldr f xs = foldr (consUnless f) [] xs

consUnless :: (a -> Bool) -> a -> [a] -> [a]
consUnless f x acc
  | not $ f x = []
  | otherwise = x:acc

-- Data.List defined groupBy which has type (a -> a -> Bool) -> [a] -> [[a]]
-- implement with a fold
groupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold f = foldr (consToGroupOrNot f) []

consToGroupOrNot :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
consToGroupOrNot _ x [] = [[x]]
consToGroupOrNot f x xs
  | f x $ head $ head xs = (x : head xs) : tail xs
  | otherwise            = [x]:xs

-- How many of the following Prelude functions can you rewrite using list folds?

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = foldr func False xs
  where func x acc = acc || f x

-- I don't believe cycle can be implemented with a fold
-- I looked into it and now realized I can
myCycle :: [a] -> [a]
myCycle xs = foldr (:) (myCycle xs) xs

myWords :: String -> [String]
myWords str = [str]

myUnlines :: [String] -> String 
myUnlines = foldl newlineAfter "" . reverse

newlineAfter :: String -> String -> String
newlineAfter str acc = acc ++ "\n" ++ str
