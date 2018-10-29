# CSCI4230-MakeupExam
##Question 1:


##Question 2:
My writeup of the answers can be found in "Q2.pdf".  The following Haskell functions will produce the same answers:

10-12:	`pointsOnCurve (EllipticCurve 1 6 :: ECC 11)`

10-14:	`map (~* Point (EllipticCurve 1 6 :: ECC 11) 2 7) [2..13]`

10-15A:	`7 ~* Point (EllipticCurve 1 6 :: ECC 11) 2 7`

10-15B: `let e = EllipticCurve 1 6 :: ECC 11 in encrypt (Point e 2 7) 5 7 (Point e 10 7)`

##Question 3:
My code for part A and B is in "CryptoMakeupExam.hs", compiling that and running the main function will give the output:
```
3a:
31531 is prime
520482 is not prime
485827 is prime
15485863 is prime

3b:
31531 has no factors
520482 has 3 as a factor
485827 has no factors
15485863 has no factors
```

Equivalent output can be formed by entering `mapM (millerRabin 10) [31531, 520482, 485827, 15485863]` and `map (pollardRho $ x^2+1) [31531, 520482, 485827, 15485863]` into the interpreter
