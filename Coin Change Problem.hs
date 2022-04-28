{-Solution to the countdown problem in haskell -}

module Main where
import Control.Monad (when)


data Op = Add | Sub | Mul | Div

data Expr = Val Int | Ap Op Expr Expr

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show Expr where
    show (Val n) = show n
    show (Ap op l k) = brak l ++ show op ++ brak k
        where
            brak (Val n) = show n
            brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (Ap op p e) = values p ++ values e

valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = (x > 0) && (y> 0)
valid Div x y = (x `mod` y) == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x+y
apply Sub x y = x-y
apply Mul x y = x*y
apply Div x y = x `div` y

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (Ap o l r) = [  apply o x y | x <- eval l, y <- eval r, valid o x y]

-- Combinatorial Functions

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat $ ((map (interleave x)) (perms xs))

choices :: [a] -> [[a]]
choices = concatMap perms . subs
choices' :: [a] -> [[a]]
choices' xs = [z | y <- subs xs, z <- perms y ]

solution :: Expr -> [Int] -> Int -> Bool
solution expr vs n = elem (values expr) (choices vs)  && eval expr == [n]
--- How do we find the solution now
-- generate all possible combinations of expressions and combine using the previously defined operators

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls, rs) | (ls, rs) <- split xs]

operator :: [Op]
operator = [Sub, Add, Mul, Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [Ap op l r | op <- operator]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs values = [ e | (ll, rr) <- split values, l <- exprs ll, r <- exprs rr, e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions values k = [y | x <- choices values, y <- exprs x, eval y == [k]]

main :: IO ()
main = print $ solutions [1,2,3] 5 