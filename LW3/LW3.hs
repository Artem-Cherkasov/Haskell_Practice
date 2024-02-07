module LW3 where

listNums :: Int -> [Int]
listNums 0 = []
listNums n = if n <= 0 then listNums 0 else n : listNums(n-1)

secondLastList :: [[a]] -> [a]
secondLastList [] = []
secondLastList (x : xs) = if not(null x) then last x : secondLastList xs else secondLastList xs

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] b = b
myUnion a [] = a
myUnion x (y:ys) = if y `elem` x then myUnion x ys else myUnion x ys ++ [y]

mySubst :: Eq a => [a] -> [a] -> [a]
mySubst [] b = []
mySubst a [] = a
mySubst (x:xs) y = if x `elem` y then mySubst xs y else x : mySubst xs y

myFunc :: Eq a => [[a]] -> Int -> [a]
myFunc [] n = []
myFunc (x:xs) n = if not(null x) && length x - 1 >= n then map (!! n) [x] ++ myFunc xs n else myFunc xs n