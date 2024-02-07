import Data.Set hiding (elems)
import qualified Data.Set as Set hiding (elems)
import Data.Map hiding ((\\))
import qualified Data.Map as Map hiding ((\\))
import Data.List hiding ((\\))
import qualified Data.List as List hiding ((\\))

listConcat :: [[a]] -> [a]
listConcat [] = []
listConcat (x:xs) = x ++ listConcat xs

charIsLower :: Char -> Bool
charIsLower x = x `elem` letters
    where letters = ['a'..'z']

mapMember :: Ord k => k -> Map k a -> Bool
mapMember k xs = k `elem` keys xs 

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection xs ys = Set.filter (`Set.member` ys) xs

listIntersperse :: Eq a => a -> [a] -> [a]
listIntersperse _ [] = []
listIntersperse x (y:ys) = if length ys /= 0 then y : x : listIntersperse x ys else y : listIntersperse x ys

setNull :: Set a -> Bool
setNull x = Set.size x == 0

mapNotMember :: Ord k => k -> Data.Map.Map k a -> Bool
mapNotMember x m = not $ any (x==) (Data.Map.keys m)

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert k a m = m `Data.Map.union` Map.singleton k a

mapSize :: Map k v -> Int
mapSize m = length $ elems m

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference xs ys = xs \\ ys

setDifference2 :: Ord a => Set a -> Set a -> Set a
setDifference2 xs ys = Set.filter p xs
    where p = (`notElem` ys)

main :: IO()
main = do    
    print(Data.List.intersperse '.' "BOAT")