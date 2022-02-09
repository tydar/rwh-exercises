-- ch04 exercises

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
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = safeInit xs >>= Just . (:) x 
