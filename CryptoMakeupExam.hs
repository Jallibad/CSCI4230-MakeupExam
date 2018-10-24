{-# LANGUAGE ViewPatterns #-}

import MathFunctions
import Polynomial



pollardRho :: (Integral a, Integral b) => a -> Polynomial b a -> Maybe a
pollardRho n p
	| d == n = Nothing
	| otherwise = Just d
	where
		g = flip mod n . eval p
		xs = tail $ iterate g 2
		ys = tail $ iterate (g . g) 2
		d:_ = dropWhile (==1) $ zipWith (\x -> gcd n . (x-)) xs ys