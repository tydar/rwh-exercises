-- file: ch02/myDrop.hs

myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

matchDrop :: Int -> [a] -> [a]
matchDrop _ [] = []
matchDrop n xs = if n <= 0 then xs else myDrop (n - 1) (tail xs)
