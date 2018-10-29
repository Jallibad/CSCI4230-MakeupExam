{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE FlexibleContexts	#-}
{-# LANGUAGE TupleSections		#-}
{-# LANGUAGE ViewPatterns		#-}

import Control.Monad (forM)
import EllipticCurve
import MathFunctions (millerRabin, pollardRho)
import Polynomial (Polynomial, eval, x)

main = do
	let q3Integers = [31531, 520482, 485827, 15485863] :: [Integer]
	putStrLn "3a:"
	forM q3Integers $ \x -> do
		putStr $ show x
		isPrime <- millerRabin 10 x
		putStrLn $ if isPrime then
			" is prime"
		else
			" is not prime"
	putStrLn ""
	
	putStrLn "3b:"
	forM q3Integers $ \n -> do
		putStr $ show n
		let putativeFactor = pollardRho (x^2+1) n
		putStrLn $ maybe " has no factors" (\f -> " has " ++ show f ++ " as a factor") putativeFactor

q2 :: IO ()
q2 = do
	let e1 = EllipticCurve 2 1 :: EllipticCurve Integer 7
	let e2 = EllipticCurve 1 7 :: EllipticCurve Integer 11
	let g = Point e2 3 2
	putStrLn "2.10-14:"
	print $ tail $ scanl1 (~+) $ replicate 13 g

	putStrLn "2.10.15a"
	let pB = 7 ~* g
	print pB

	putStrLn "2.10.15b"
	let k = 5
	let nB = 7
	let pM = Point e2 10 7
	print $ encrypt g k nB pM

type ECC p = EllipticCurve Integer p

--insertNondisplayingSpaces