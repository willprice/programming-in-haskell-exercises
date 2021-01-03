data Nat = Zero | Succ Nat


nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n-1)

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Succ n) = Succ (add a n)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult a (Succ Zero) = a
mult a (Succ b) = add (mult a b) a


data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) 
    | x == y = True
    | x < y = occurs x l
    | otherwise = occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
    EQ -> True
    LT -> occurs x l
    GT -> occurs x r

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
    deriving Show


countLeaves :: Tree' a -> Int
countLeaves (Leaf' _) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = (abs (countLeaves l - countLeaves r) <= 1) && balanced l && balanced r

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance ls) (balance rs)
    where (ls, rs) = halve xs

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)
folde f g (Mult e1 e2) = g (folde f g e1) (folde f g e2)

eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

--instance Eq a => Eq (Maybe a) where
--    (Just a) == (Just b) = a == b
--    Nothing == Nothing = True
--    Nothing == (Just _) = False
--    (Just _) == Nothing = False

--instance Eq a => Eq [a] where
--    [] == ys = null ys
--    xs == [] = null xs
--    (x:xs) == (y:ys) = y == x && (xs == ys)

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = Equiv (Var 'A') (Not (Var 'A'))

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)  = x : filter (/= x) (rmdups xs)


eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s (Imply p q) && eval s (Imply q p)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2
vars (Equiv p1 p2) = vars p1 ++ vars p2

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools $ length vs)
    where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

type Cont = [Op]
data Op = EVALA Expr | EVALM Expr | ADD Int | MULT Int

eval'' :: Expr -> Cont -> Int
eval'' (Val n) c = exec c n
eval'' (Add x y) c = eval'' x (EVALA y : c)
eval'' (Mult x y) c = eval'' x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA y : c) n = eval'' y (ADD n : c)
exec (EVALM y : c) n = eval'' y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value :: Expr -> Int
value e = eval'' e []