module AufgabeFFP1 where

-- 1 --

-- powers of 2, starting at 0
pof2s :: [Integer]
pof2s = 1 : map (2*) pof2s -- 2^n = 2^(n-1) * 2

-- 2 --

{- Generates rows of pascal triangle. -}

pd :: [[Integer]]
pd = base : pd' base
    where base = [1]

{- Generates row of pascal triangle by shifting
 - and adding previous row as follows:
 -
 - l = [1, 2, 1]
 -
 -   a = [0, 1, 2, 1]
 - + b = [1, 2, 1, 0]
 - ----------------
 - cur = [1, 3, 3, 1]
 -
 - Recurses infinitely. -}

pd' :: [Integer] -> [[Integer]]
pd' l = cur : pd' cur
    where a = 0 : l
          b = l ++ [0]
          cur = zipWith (+) a b

-- 3 --

-- summands for fib(n), from the diagonals of Pascal's triangle
fibdiag :: Integer -> [Integer]
-- Needs reverse, because diag only calculates the main diagonal.
fibdiag n = (diag . reverse . take (fromIntegral n)) pd
  -- Get the main diagonal of the matrix.
  where diag :: [[a]] -> [a]
        diag [] = []
        diag ([]:_) = []
        diag (x:xs) = head x : (diag . map (drop 1)) xs

-- 4 --

-- stream of Fibonacci diagonals
fibdiags :: [[Integer]]
fibdiags = [ fibdiag x | x <- [1..] ]

-- 5 --

-- stream of Fibonacci numbers
fibspd :: [Integer]
fibspd = map sum fibdiags
