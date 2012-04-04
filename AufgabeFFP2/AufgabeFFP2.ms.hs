module AufgabeFFP2 where

-- 1 --

-- sieve as seen in the lecture
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [ y | y <- xs, mod y x > 0 ]

-- primes as seen in the lecture, a stream of all primes
primes :: [Integer]
primes = sieve [2 ..]

-- a stream of prime pairs
pps :: [(Integer,Integer)]
pps = filter (\(x,y)->y==x+2) (zipWith (\x y->(x,y)) primes (tail primes))

-- 2 --

-- slow pow function, calculates 2^n slowly
pow :: Int -> Integer
pow 0 = 1
pow n = pow (n-1) + pow (n-1)

-- a list of powers of 2 starting with 2^0 (1)
pfList = [ powFast x | x<-[0..] ]

-- fast pow function, calculates 2^n fast
powFast :: Int -> Integer
powFast n | n <= 0 = 1
-- utilize pfList to speed things up
powFast n = pfList!!(n-1) + pfList!!(n-1)

-- 3 --

-- stream of factorials
fList = [ fac x | x<-[0..] ]

-- calculate factorial of n
fac :: Int -> Integer
fac 0 = 1
fac n = fList!!(n-1) * (fromIntegral n)

-- helper function h as defined in the assignment
h :: Int -> Int -> Integer
h z i = ((fromIntegral z^i) `div` (fac i))

-- the slow g(z,k) function
f :: Int -> Int -> Integer
--f z k = sum [ h z i | i<-[0..k] ]
f _ 0 = 1
f _ k | k < 0 = 0
f z k = (f z (k-1)) + (h z k)

-- the fast g(z,k) function
fMT :: Int -> Int -> Integer
fMT _ 0 = 1
fMT _ k | k < 0 = 0
fMT z k = (gList!!(k-1)) + (h z k)
  where gList = [ fMT z i | i<-[0..] ]

-- 4 --

-- gets a list containing the digits of n
digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

-- calculates the Gödel-Number of n
gz :: Integer -> Integer
gz n
  | n > 0     = product (zipWith (\x y->x^y) primes (digits n))
  | otherwise = 0

-- a stream of the Gödel-Numbers of all positive integers
gzs :: [Integer]
gzs = map gz [1..]
