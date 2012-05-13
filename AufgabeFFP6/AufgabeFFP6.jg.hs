{-# LANGUAGE FlexibleInstances #-}

module AufgabeFFP6 where

import Data.Array

{- Given an int array and an operator array, returns the result
 - received by combininig both as follows:
 -
 - [1,2,3,4,5], [+,-,+,*] =
 - ((((1 + 2) - 3) + 4) * 5) = 20
 -
 - Divide by zero results in an error. -}
eval :: Array Int Int -> Array Int (Int -> Int -> Int) -> Int
eval = undefined

{- Given an int array and an int result, returns all possible operator
 - arrays such that eval array op == result.
 - yield_bt accomplishes this using backtracking, while
 - yield_gtf uses generate/transform/filter functions. -}
yield_bt :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_bt = undefined

yield_gtf :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_gtf = filt . transform . generate

filt = undefined
transform = undefined
generate = undefined

(./.) :: Int -> Int -> Int
(./.) = div

instance Show (Int -> Int -> Int) where
    show _ = "TODO"
