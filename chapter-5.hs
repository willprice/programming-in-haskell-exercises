module Chapter5 where

import Data.Char


positions :: Eq a => a -> [a] -> [Int]
positions needle haystack = [
     i | (x, i) <- zip haystack [0..], needle == x]

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

letter2int :: Char -> Int
letter2int c = ord c - ord 'a'

int2letter :: Int -> Char
int2letter i = chr $ i + ord 'a'

shift :: Int -> Char -> Char
shift n c | isLower c = int2letter $ (letter2int c + n) `mod` 26
          | otherwise = c

encode :: Int -> String -> String        
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0
        , 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0
        , 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1
        ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o-e)^2 / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) lowercasexs
    where 
        factor = head $ positions (minimum chitab) chitab
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs lowercasexs
        lowercasexs = (map toLower xs)



-- Exercises

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate count val = [val | _ <- [0..count - 1]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum . tail . reverse . factors $ x) == x]

ex7correct = (concat [[(x, y) | y <- [3,4]] | x <- [1,2]]) == [(x,y) | x <- [1,2], y <- [3,4]]

find :: Eq a => a -> [(a,b)] -> [b]
find key dict = [val | (key', val) <- dict, key == key']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]