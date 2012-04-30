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
minIndex = undefined
