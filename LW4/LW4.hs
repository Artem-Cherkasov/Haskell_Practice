module LW4 where

import Data.Set hiding (null)
import qualified Data.Set as Set hiding (null)
import Data.Map hiding (null)
import qualified Data.Map as Map hiding (null)

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
listIntersperse x (y:ys) = if not (null ys) then y : x : listIntersperse x ys else y : listIntersperse x ys
