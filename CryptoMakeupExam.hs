{-# LANGUAGE DataKinds		#-}
{-# LANGUAGE TupleSections	#-}
{-# LANGUAGE ViewPatterns	#-}

import Control.Arrow ((***), (&&&))
import Control.Monad (forM)
import Data.List (genericReplicate, genericTake)
import EllipticCurve
import MathFunctions (isqrt)
import ModularArithmetic
import Polynomial (Polynomial, eval, x)
import System.Random (Random, randomRIO)

--modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow 0 _ _ = 1
modularPow e m b = if odd e then mult result b else result
	where
		result = modularPow (e `div` 2) m (mult b b)
		mult x y = (x*y) `mod` m

millerRabin :: (Enum a, Num a, Integral b, Random b) => a -> b -> IO Bool
millerRabin k n
	| n > 3 && odd n = and <$> forM [1..k] (const witness)
	| otherwise = return False
	where
		repeatSquares' = (. iterate (\x -> (x^2) `mod` n)) . genericTake
		witness' d = test . modularPow d n <$> randomRIO (2,n-2)
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
		d:_ = dropWhile (==1) $ zipWith (\x -> gcd n . (x-)) xs ys

main = do
	putStrLn "2.10-14:"
	let eP = EllipticCurve 1 7 :: EllipticCurve Integer 11
	print $ tail $ scanl1 (~+) $ replicate 13 $ (Point eP 3 2)

	let q3Integers = [31531, 520482, 485827, 15485863] :: [Integer]
	putStrLn "3a:"
	forM q3Integers $ \x -> do
		putStr $ show x
		isPrime <- millerRabin 10 x
		if isPrime then
			putStrLn " is prime"
		else
			putStrLn " is not prime"
	putStrLn ""
	
	putStrLn "3b:"
	forM q3Integers $ \n -> do
		putStr $ show n
		let putativeFactor = pollardRho (x^2+1) n
		putStrLn $ maybe " has no factors" (\f -> " has " ++ show f ++ " as a factor") putativeFactor 