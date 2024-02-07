module LW2 where

doMyList :: Int -> [Int]
doMyList n = [n..2*n-1]

oddEven [a] = [a]
oddEven (x:y:xs) = y: x: oddEven xs
oddEven [] = []

insert :: [a] -> a -> Int -> [a]
insert x y pos = xf ++ [y] ++ xs
    where (xf, xs) = (fst(splitAt pos x), snd(splitAt pos x))


listSumm :: [Int] -> [Int] -> [Int]
listSumm (x:xs) (y:ys) = (x + y): listSumm xs ys
listSumm _ [] = []
listSumm [] _ = []

position :: Eq a => [a] -> a -> Int
position (x:xs) y = if x == y then 1 else 1 + position xs y

myFunc1 :: Int -> Int
myFunc1 n = sum [i..n]
    where i = 1

myFunc2 :: Int -> Int
myFunc2 n = sum [n-i, n-(i+1)..n-n]
    where i = 1
