module MathFunctions (
	millerRabin,
	pollardRho
	) where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.List (genericTake, genericReplicate)
import Data.Maybe (maybe)
import Polynomial (Polynomial, eval)
import System.Random (Random, randomRIO)

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b else result
	where
		result = modularPow (mult b b) (e `div` 2) m
		mult x y = (x*y) `mod` m

modularPow' 0 _ _ = 1
modularPow' e m b = if odd e then mult result b else result
	where
		result = modularPow' (e `div` 2) m (mult b b)
		mult x y = (x*y) `mod` m

millerRabin :: (Integral a, Integral b, Random b) => a -> b -> IO Bool
millerRabin k n = foldl1 (liftA2 (&&)) $ baseCondition : genericReplicate k witness
	where
		baseCondition = return $ n > 3 && odd n && k > 0
		repeatSquares' = (. iterate (\x -> (x^2) `mod` n)) . genericTake
		witness' d = test . modularPow' d n <$> randomRIO (2,n-2)
		(repeatSquares, witness) = (repeatSquares' *** witness') $ factorOddPowers $ n-1
		test x = x==1 || any (==n-1) (repeatSquares x)

factorOddPowers :: Integral a => a -> (a,a)
factorOddPowers = until (odd . snd) ((+1) *** (`div` 2)) . (0,)

pollardRho :: (Integral a, Integral b) => Polynomial b a -> a -> Maybe a
pollardRho p n
	| d == n = Nothing
	| otherwise = Just d
	where
		g = flip mod n . eval p
		xs = tail $ iterate g 2
		ys = tail $ iterate (g . g) 2
		d = head $ dropWhile (==1) $ zipWith ((gcd n .) . (-)) xs ys