module ListFunctions where

import Data.List
import Data.Foldable hiding (all, find)
import Data.Array (indices, elems, listArray, (//), (!))
import Data.Maybe (fromJust)
import Data.Function
import Control.Arrow
import qualified Data.Vector as Vec

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs)
	| f x = [x]
	| otherwise = x : takeUntil f xs

takeLast :: Int -> [a] -> [a]
takeLast number list = drop (length list - number) list

isPalindrome :: Eq (a) => [a] -> Bool
isPalindrome xs = first == second
	where
		half = (length xs) `div` 2
		first = take half xs
		second = take half $ reverse xs

--Taken from Data.List.Ordered
orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case compare x y of
          LT -> x : loop xs (y:ys)
          EQ -> x : loop xs ys
          GT -> y : loop (x:xs) ys

differences [] = []
differences (_:[]) = []
differences (x:y:xs) = (y-x) : differences (y:xs)

diffs _ [] = []
diffs _ (_:[]) = []
diffs f (x:y:xs) = (f x y) : diffs f (y:xs)

allEqual :: Eq (a) => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick n (x:xs) = (map (x:) $ pick (n-1) xs)++(pick n xs)

circulateList :: [a] -> [[a]]
circulateList list = tail $ zipWith (++) (tails list) (inits list)

orderedPermutations :: Ord (a) => [a] -> [[a]]
orderedPermutations xs = xs : (nextPermutation $ listArray (0, n) xs)
	where
		nextPermutation a = case find (\x -> (a ! x)<(a ! (x+1))) [n-1, n-2.. 0] of
			Nothing -> []
			Just k -> answer : (nextPermutation $ listArray (0, n) answer)
				where
					l = head $ filter (\x -> (a ! k)<(a ! x)) [n,n-1..k+1]
					answer = uncurry (++) $ second reverse $ splitAt (k+1) $ elems $ a // [(k, a ! l), (l, a ! k)]
		n = length xs - 1

nextPermutation :: Ord (a) => Vec.Vector a -> Maybe (Vec.Vector a)
nextPermutation a = fmap ans $ find (\x -> (a Vec.! x)<a Vec.! (x+1)) [n-1, n-2.. 0]
	where
		n = Vec.length a
		ans k = Vec.backpermute a $ Vec.fromList ([0..k]++[])
			where
				l = head $ filter (\x -> (a Vec.! k)<(a Vec.! x)) [n, n-1.. k+1]

takePairs :: [a] -> [(a,a)]
takePairs [] = []
takePairs (_:[]) = []
takePairs (x1:x2:xs) = (x1,x2) : takePairs (x2:xs)