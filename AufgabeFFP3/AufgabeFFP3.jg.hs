module AufgabeFFP3 where

import Data.List (delete)

type Weight = Int
type Value = Int
type Item = (Weight, Value)
type Items = [Item]
type Load = [Item]
type Loads = [Load]
type LoadWghtVal = (Load, Weight, Value)
type MaxWeight = Weight

{-- Generates all possible combinations of items. --}

generator :: Items -> Loads
generator = select

{-- Returns all possible selections of elements in a. --}

select :: Eq a => [a] -> [[a]]
select (x:xs) = [[x]] ++ ys ++ [ x : y | y <- ys ]
    where ys = select (delete x xs)
select _ = []

{-- Calculates total weight and value for each load. --}

transformer :: Loads -> [LoadWghtVal]
transformer = map metrics

{-- Calculates weight and value for a single load. --}

metrics :: Load -> LoadWghtVal
metrics = foldr metric ([], 0, 0)

{-- Takes a LoadWghtVal, and updates it with the current item. --}

metric :: Item -> LoadWghtVal -> LoadWghtVal
metric l@(w, v) (l', w', v') = (l:l', w + w', v + v')

{-- Filters input to combinations which don't exceed the maximum
 - weight. --}

filter :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
filter w xs = Prelude.filter (\(_, w', _) -> w' <= w) xs

{-- Selects combination(s) with the highest value. --}

selector1 :: [LoadWghtVal] -> [LoadWghtVal]
selector1 xs = Prelude.filter (\(_, _, v) -> v == maxval xs) xs

maxval :: [LoadWghtVal] -> Int
maxval xs = maximum $ [ v | (_, _, v) <- xs ]

minwgt :: [LoadWghtVal] -> Int
minwgt xs = minimum $ [ w | (_, w, _) <- xs ]

{-- Selects combination(s) which achieve the highest value with
 - minimal weight. --}

selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 xs = Prelude.filter (\(_, w, v) -> w == minwgt xs') xs'
    where xs' = selector1 xs

{-- Binomial coefficients, naive implementation. --}

binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)

{-- Binomial coefficients, stream implementation. --}

binomS :: (Integer,Integer) -> Integer
binomS (n, k)
    | n < 0 || k < 0 || n < k = 0
    | otherwise = (pascal !! n') !! k'
    where n' = fromIntegral n
          k' = fromIntegral k

{-- Stream of pascal triangle rows. --}

pascal :: [[Integer]]
pascal = [1] : pascal' [1]

pascal' :: [Integer] -> [[Integer]]
pascal' xs = next : pascal' next
    where next = 1 : zipWith (+) xs (tail xs) ++ [1]

{-- Binomial coefficients, memoization implementation. --}

binomM :: (Integer,Integer) -> Integer
binomM (n, k)
    | n < 0 || k < 0 || n < k = 0
    | otherwise = (pascaltable !! n') !! k'
    where n' = fromIntegral n
          k' = fromIntegral k

{-- Memo table of pascal triangle rows. --}

pascaltable :: [[Integer]]
pascaltable = [ pascalmemo n | n <- [0 ..] ]

pascalmemo :: Int -> [Integer]
pascalmemo 0 = [1]
pascalmemo n = 1 : zipWith (+) prev (tail prev) ++ [1]
    where prev = pascaltable !! (n - 1)
