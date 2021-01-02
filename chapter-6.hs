fac :: Int -> Int
fac n | n == 0 = 1
      | n > 0 = n * fac (n - 1)
      | otherwise = 0

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(^) :: Int -> Int -> Int
x ^ 1 = x
x ^ y = x * (x Main.^ (y-1)) 

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid (x - y) y
           | y > x = euclid x (y - x)

-- length [1, 2, 3]
-- 1 + length [2, 3]
-- 1 + 1 + length [3]
-- 1 + 1 + 1 + length []
-- 1 + 1 + 1 + 0 = 3

-- drop 3 [1, 2, 3, 4, 5]
-- drop 2 [2, 3, 4, 5]
-- drop 1 [3, 4, 5]
-- drop 0 [4, 5]
-- [4, 5]

-- init [1, 2, 3]
-- 1 : (init [2, 3])
-- 1 : (2 : (init [3]))
-- 1 : (2 : [])
-- [1, 2]

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' ([]:xss) = concat' xss
concat' ((x:xs):xss) = x : concat' (xs:xss)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs Main.!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' needle haystack | haystack == [] = False
                      | head haystack == needle = True
                      | otherwise = elem' needle (tail haystack)

merge :: Ord a => [a] -> [a] -> [a]

merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs) | a > b = b : merge (a:as) bs
                    | otherwise = a : merge as (b:bs)

halve :: [a] -> ([a], [a])
halve xs = (take fstLen xs, drop fstLen xs)
    where 
        len = length xs
        isEven = len `mod` 2 == 0
        fstLen = len `div` 2


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort fstHalf) (msort sndHalf)
    where 
        (fstHalf, sndHalf) = halve xs 

sum' :: Num a => [a] -> a
sum' [x] = x
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 1 (x:xs) = [x]
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs