module AufgabeFFP3 where

type Weight = Int
type Value = Int
type Item = (Weight, Value)
type Items = [Item]
type Load = [Item]
type Loads = [Load]
type LoadWghtVal = (Load, Weight, Value)
type MaxWeight = Weight

generator :: Items -> Loads
generator = undefined

transformer :: Loads -> [LoadWghtVal]
transformer = undefined

filter :: [LoadWghtVal] -> MaxWeight -> [LoadWghtVal]
filter = undefined

selector :: [LoadWghtVal] -> [LoadWghtVal]
selector = undefined

selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 = undefined

binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)


binomS :: (Integer,Integer) -> Integer
binomS = undefined

binomM :: (Integer,Integer) -> Integer
binomM = undefined
