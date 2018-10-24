{-# LANGUAGE ViewPatterns #-}

import Polynomial

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b else result
	where
		result = modularPow (mult b b) (e `div` 2) m
		mult x y = (x*y) `mod` m

millerRabinTest :: Int -> IO Bool
millerRabinTest n = do
	let (r,d) = millerRabinFactorInput n
	--a <- randomRIO (2,n-2)
	let a = 174
	let x = modularPow a d n
	print a
	print x
	print $ take (r-1) $ iterate (\x -> modularPow x 2 n) x
	if x==1 || x == n-1 then
		return True
	else
		return $ not $ any (==n-1) $ take (r-1) $ iterate (\x -> modularPow x 2 n) x

millerRabinFactorInput :: Integral a => a -> (a,a)
millerRabinFactorInput = until (odd . snd) ((+1) *** (`div` 2)) . (0,) . pred

pollardRho :: (Integral a, Integral b) => a -> Polynomial b a -> Maybe a
pollardRho n p
	| d == n = Nothing
	| otherwise = Just d
	where
		g = flip mod n . eval p
		xs = tail $ iterate g 2
		ys = tail $ iterate (g . g) 2
		d:_ = dropWhile (==1) $ zipWith (\x -> gcd n . (x-)) xs ys