{-# LANGUAGE FlexibleInstances #-}

module AufgabeFFP6 where

import Data.Array

-- 1 --

{- Evaluates the expression as described in the specification. The lis of
 - numbers must be one longer than the list of operators and be at least 2
 - long. Otherwise the result is undefined. -}
eval :: Array Int Int -> Array Int (Int->Int->Int) -> Int
eval nums ops = recZip n nums' ops'
  where (n:nums') = elems nums
        ops' = elems ops
	recZip a [] _ = a
	recZip a (n:ns) (o:os) =  recZip (o a n) ns os

-- 2 --

type Result = Int
type Numbers = [Int]
type Operations = [Int -> Int -> Int]

{- Node for backtacking search -}
type Node = (Numbers, Operations, Result)

{- Tries to find operators such that the numbers together with them yield
 - the given result. Uses backtracking. -}
yield_bt :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield_bt nums res = sols'
  where sols = searchDfs succY goalY (nums', [], res)
        sols' = map ((listArray (1,len)) . getOp) sols
	getOp (_,o,_) = o
	nums' = elems nums
	len = length nums' - 1

{- Gets the successors of a node. A successor is a node where the list of
 - operators contains one more operator. If the operator list already contains
 - length nums - 1 elements, there are no successors. -}
succY :: Node -> [Node]
succY (nums, ops, res)
  | leno + 1 >= lenn = []
  | nums !! (lenn - leno - 1) == 0 = map (\x->(nums, x:ops, res)) [(+),(*),(-)]
  | otherwise = map (\x->(nums, x:ops, res)) [(+),(*),(-),div]
  where lenn = length nums
        leno = length ops

{- Returns true if the operators applied to the numbers in the node yield the
 - result and false otherwise. -}
goalY :: Node -> Bool
goalY (nums, ops, res) = (leno + 1) == lenn && (eval nums' ops') == res
  where nums' = listArray (1, lenn) nums
        ops' = listArray (1, leno) ops
	lenn = length nums
	leno = length ops

{- Tries to find operators such that the numbers together with them yield
 - the given result. Uses generate/transform/filter. Nums must be at least 2
 - long, otherwise the result is undefined. -}
yield_gtf :: Array Int Int -> Int -> [Array Int (Int->Int->Int)]
yield_gtf nums res = ((filt res).(transform nums).generate) nums'
  where len = ((length . elems) nums) - 1
        nums' = elems nums

{- Returns the operator list of those operator/result pairs where the result
 - equals res. -}
filt :: Int -> [(Array Int (Int->Int->Int), Int)] -> [Array Int (Int->Int->Int)]
filt res ops = (map (fst) . filter (\x->(snd x) == res)) ops

{- Calculates the result of the operators applied to the numbers and returns
 - both as a touple. The same restrictions for the number and operator arrays as
 - in eval apply. -}
transform :: Array Int Int -> [Array Int (Int->Int->Int)]
		-> [(Array Int (Int->Int->Int), Int)] 
transform nums ops = map (\x->(x, eval nums x)) ops

{- Generates all possible operator lists for the given number list (which must
 - be at least 2 long). It also avoids assigning the division to a 0. -}
generate :: [Int] -> [Array Int (Int->Int->Int)]
generate nums = map (listArray (1, len)) ops
  where ops = addoplst nums' [[]]
        len = length nums'
        addop 0 a = map (:a) [(+),(*),(-)]
        addop n a = map (:a) [(+),(*),(-),div]
	addoplst [] a = a
	addoplst (n:ns) a = ((addoplst ns) . concat . (map (addop n))) a
	nums' = tail nums

{- Types and functions for the generic DFS algorithm. -}
data Stack a = EmptyStk | Stk a (Stack a)

push x s = Stk x s

pop EmptyStk = error "pop from an empty stack"
pop (Stk _ s) = s

top EmptyStk = error "top from an empty stack"
top (Stk x _) = x

emptyStack = EmptyStk

stackEmpty EmptyStk = True
stackEmpty _ = False

searchDfs :: (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = (search' (push x emptyStack) )
  where search' s
          | stackEmpty s = []
          | goal (top s) = top s : search' (pop s)
          | otherwise = let x = top s
                        in search' (foldr push (pop s) (succ x))

-- 3 --

instance Show (Int -> Int -> Int) where
    show f | f 10 5 == 15 = "plus"
           | f 10 5 == 5 = "minus"
           | f 10 5 == 50 = "times"
           | f 10 5 == 2 = "div"
           | otherwise = error "undefined operator"
