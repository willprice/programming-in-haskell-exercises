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

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)


chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

map'' :: (a -> b) -> [a] -> [b]
map'' fn = unfold null (fn . head) (drop 1)

iterate' :: (a -> a) -> a -> [a]
iterate' fn = unfold (const False) id fn