module AufgabeFFP1 where

-- 1 --

-- powers of 2, starting at 0
pof2s :: [Integer]
pof2s = 1 : map (2*) pof2s -- 2^n = 2^(n-1) * 2

-- 2 --

-- rows of Pascal's triangle
pd :: [[Integer]]
-- We should use map but is it allowed?
pd = [1] : zipWith (\x y -> [y] ++ (next x) ++ [y]) pd [1,1..]
  -- zip previous row with itself, add 1 on the ends later
  where next :: [Integer] -> [Integer]
        next x = zipWith (+) x (tail x)

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
fibspd = map (foldl (+) 0) fibdiags
