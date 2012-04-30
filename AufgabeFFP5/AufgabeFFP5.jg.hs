module AufgabeFFP5 where

import Data.Array

{- mas, amas and lmas are all about partial sums of integer arrays.
 -
 - A partial sum over an array and an index range is the sum of all elements
 - in the specified range. -}

{- mas returns the maximal partial sum of the array. -}
mas :: Array Int Int -> Int
mas = undefined

{- amas returns all index ranges with partial sum = mas. -}
amas :: Array Int Int -> [(Int, Int)]
amas = undefined

{- lmas returns the longest range of amas. -}
lmas :: Array Int Int -> (Int, Int)
lmas = undefined

{- minIndex returns the smallest index of an element satisfying f. -}
minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array f = minimum $ divideAndConquer mi_indiv (mi_solve f) mi_divide
                                              mi_combine (array, bounds array)

{- Individual element reached once the rangeSize is equal to 1. -}
mi_indiv :: (Ix a) => (Array a b, (a, a)) -> Bool
mi_indiv (_, index) = rangeSize index == 1

{- If the element satisfies f, the add its index to the solution space,
 - otherwise return an empty list. -}
mi_solve :: (Ix a) => (b -> Bool) -> (Array a b, (a, a)) -> [a]
mi_solve f (array, (i0, _))
    | f (array ! i0) = [i0]
    | otherwise = []

{- Divide by splitting index range in middle. -}
mi_divide :: (Ix a) => (Array a b, (a, a)) -> [(Array a b, (a, a))]
mi_divide (array, (i0, i1)) = [ (array, (i0, mid)), (array, (mid', i1)) ]
    where mid = range (i0, i1) !! ((rangeSize (i0, i1) `div` 2) - 1)
          mid' = range (i0, i1) !! (rangeSize (i0, i1) `div` 2)

{- Combine is a simple concat of all valid indices. -}
mi_combine :: (Ix a) => p -> [[a]] -> [a]
mi_combine _ xs = concat xs

divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s)
      -> p -> s
divideAndConquer ind solve divide combine initPb = dc' initPb
    where dc' pb
              | ind pb  = solve pb
              | otherwise = combine pb (map dc' (divide pb))
