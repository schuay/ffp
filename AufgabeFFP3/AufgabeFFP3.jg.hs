module AufgabeFFP3 where

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
generator = undefined

{-- Calculates total weight and value for each load. --}

transformer :: Loads -> [LoadWghtVal]
transformer = undefined

{-- Filters input to combinations which don't exceed the maximum
 - weight. --}

filter :: [LoadWghtVal] -> MaxWeight -> [LoadWghtVal]
filter = undefined

{-- Selects combination(s) with the highest value. --}

selector1 :: [LoadWghtVal] -> [LoadWghtVal]
selector1 = undefined

{-- Selects combination(s) which achieve the highest value with
 - minimal weight. --}

selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 = undefined

{-- Binomial coefficients, naive implementation. --}

binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)

{-- Binomial coefficients, stream implementation. --}

binomS :: (Integer,Integer) -> Integer
binomS = undefined

{-- Binomial coefficients, memoization implementation. --}

binomM :: (Integer,Integer) -> Integer
binomM = undefined
