module AufgabeFFP1 where

{- Generates 2^n, n <- [ 0 .. ]. -}

pof2s :: [Integer]
pof2s = 1 : map (*2) pof2s

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

{- Returns n'th diagonal of pascal triangle,
 - or empty list if n < 1.
 - n is 1-based. -}

fibdiag :: Integer -> [Integer]
fibdiag n
    | n < 1 = []
    | otherwise = fibdiag' 0 (take m pd)
    where m = fromIntegral n

{- Constructs diagonal of triangle. -}

fibdiag' :: Int -> [[Integer]] -> [Integer]
fibdiag' n rows
    | rows == [] = []
    | n >= length cur = []
    | otherwise = (cur !! n) : fibdiag' (n + 1) next
    where cur = last rows
          next = init rows

{- Generates diagonals of pascal triangle. -}

fibdiags :: [[Integer]]
fibdiags = map fibdiag [1..]

{- Generates fibonacci sequence. -}

fibspd :: [Integer]
fibspd = map sum fibdiags
