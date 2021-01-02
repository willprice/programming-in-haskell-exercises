import Data.Char

type Bit = Int

-- [f x | x <- xs, p x]
-- = filter p $ map f xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' predicate (x:xs) = predicate x && all' predicate xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' predicate (x:xs) = predicate x || any' predicate xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' predicate (x:xs) 
    | predicate x = x : takeWhile' predicate xs
    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' predicate (x:xs)
    | predicate x = dropWhile' predicate xs
    | otherwise = x:xs

map' :: (a -> b) -> [a] -> [b]
map' fn = foldr (\x acc -> fn x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate = foldr accFn []
    where accFn x ls | predicate x = x : ls
                     | otherwise = ls

dec2int :: [Int] -> Int
dec2int = foldl accFn 0
    where accFn acc i = acc * 10 + i

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' fn = \a b -> fn (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' fn = \(a, b) -> fn a b

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

bin2int binaryStr = foldr (\bit acc -> 2 * acc + bit) 0 binaryStr
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParityBit) . (chopn 9)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chopn :: Int -> [Bit] -> [[Bit]]
chopn n = unfold null (take n) (drop n)

computeParity :: [Bit] -> Bit
computeParity xs = (length $ filter (== 1) xs) `mod` 2

addParityBit :: [Bit] -> [Bit]
addParityBit xs = (computeParity xs) : xs

checkParityBit :: [Bit] -> [Bit]
checkParityBit (parityBit:bits) 
    | computeParity bits == parityBit = bits 
    | otherwise = error "Parity bit check failed"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
-- Exercise 8: drop parity bit to simulate comms failure
--channel = tail

map'' :: (a -> b) -> [a] -> [b]
map'' fn = unfold null (fn . head) (drop 1)

iterate' :: (a -> a) -> a -> [a]
iterate' fn = unfold (const False) id fn

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap fnEven fnOdd xs = altMapEven xs
    where 
        altMapEven [] = []
        altMapEven (x:xs) = fnEven x : altMapOdd xs
        altMapOdd [] = []
        altMapOdd (x:xs) = fnOdd x : altMapEven xs

luhnDouble :: Int -> Int
luhnDouble n 
    | m > 9 = m - 9
    | otherwise = m
    where m = n * 2

luhn :: [Int] -> Bool 
luhn ns = sum (altMap id luhnDouble (reverse ns)) `mod` 10 == 0