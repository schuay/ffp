module AufgabeFFP2 where

{- Generates a stream of prime twins. -}

pps :: [(Integer, Integer)]
pps = filter (\(a,b) -> a + 2 == b) pairs
    where pairs = zip primes (tail primes)

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [ y | y <- xs, y `mod` x > 0 ]

{- Calculates 2^n using memoization. -}

powFast :: Int -> Integer
powFast n
    | n < 0 = 1
    | otherwise = map pow [0 ..] !! n
    where pow 0 = 1
          pow n = 2 * pow (n - 1)

{- f and fMT calculate the formula
 -
 - 1 + z + z^2/2! + z^3/3! + ... + z^k/k!
 -
 - fMT uses memo tables while f doesn't. -}

f :: Int -> Int -> Float
f z k = sum $ map (h z) [0..k]

fMT :: Int -> Int -> Float
fMT _ k | k < 0 = 0
fMT z k = map fMT' [0..] !! k
    where fMT' 0 = 1
          fMT' i = fMT' (i - 1) + h z i

h :: Int -> Int -> Float
h z i = fromIntegral (z'^i) / fromIntegral (fac i)
    where z' = fromIntegral z

facs :: [Integer]
facs = scanl (*) 1 [1..]

fac :: Int -> Integer
fac n = facs !! n

{- Returns, respectively, the Goedel number
 - of n, and the stream of Goedel numbers
 - for [1..]. -}

gz :: Integer -> Integer
gz n | n < 1 = 0
gz n = product $ zipWith (^) primes (digits n)

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

gzs :: [Integer]
gzs = map gz [1..]
