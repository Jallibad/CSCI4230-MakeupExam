module EllipticCurve where

import Data.List (genericReplicate)
import MathFunctions (isqrt)
import ModularArithmetic

data EllipticCurve n p = EllipticCurve {_a :: Mod n p, _b :: Mod n p} deriving (Eq)

instance (Show n, ValidMod n p) => Show (EllipticCurve n p) where
	show (EllipticCurve a b) = "y^2 = x^3" ++ (showTerm a) ++ "x" ++ (showTerm b)
		where
			showTerm t
				| t >= 0 = "+" ++ (show $ unMod t)
				| otherwise = "-" ++ (show $ unMod t)

data Point n p = Point {_curve :: EllipticCurve n p, _x :: Mod n p, _y :: Mod n p}

instance Show n => Show (Point n p) where
	show (Point _ x y) = show (x,y)

slope :: ValidMod n p => Point n p -> Point n p -> Mod n p
slope (Point curve1 x1 y1) (Point curve2 x2 y2)
	| curve1 /= curve2 = error "Points on different curves"
	| x1 == x2 = (3*x1^2+(_a curve1)) `div` (2*y1)
	| otherwise = (y1-y2) `div` (x1-x2)

evalCurve :: ValidMod n p => EllipticCurve n p -> Mod n p -> Mod n p
evalCurve (EllipticCurve a b) x0 = isqrt $ x0^3+a*x0+b

infixl 6 ~+
(~+) :: ValidMod n p => Point n p -> Point n p -> Point n p
p@(Point curvep xp yp) ~+ q@(Point curveq xq yq) = Point curvep xr yr
	where
		m = slope p q
		xr = m^2-xp-xq
		yr = m*(xp-xr)-yp

infixr 7 ~*
(~*) :: ValidMod n p => n -> Point n p -> Point n p
0 ~* (Point curve _ _) = Point curve 0 0
1 ~* p = p
2 ~* p = p ~+ p
n ~* p
	| odd n = p ~+ (n-1) ~* p
	| otherwise = (n `div` 2) ~* 2 ~* p
