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
eval nums ops = eval' nums' ops' ind
    where nums' = elems nums
          ops' = elems ops
          ind = length ops' - 1

{- Wraps up the lists from right to left and returns overall result.
 - Assumes that lists are matched, meaning operator list size = int list
 - size - 1. -}
eval' :: [Int] -> [Int -> Int -> Int] -> Int -> Int
eval' nums ops ind
    | ind >= 0 = f x y
    | otherwise = head nums
    where f = ops !! ind
          x = eval' nums ops (ind - 1)
          y = nums !! (ind + 1)

{- Current op sequence, int sequence, desired value. -}
type Node = ([Int -> Int -> Int], [Int], Int)

{- Given an int array and an int result, returns all possible operator
 - arrays such that eval array op == result.
 - yield_bt accomplishes this using backtracking, while
 - yield_gtf uses generate/transform/filter functions. -}
yield_bt :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_bt nums result = map extract solutions
    where solutions = searchDfs succs goal ([], elems nums, result)
          extract (ops, _, _) = listArray (0, length ops - 1) ops

oplist = [(+), (-), (*), (./.)]

succs :: Node -> [Node]
succs (ops, nums, v')
    | length ops + 1 >= length nums = []
    | otherwise = [ (op:ops, nums, v') | op <- opl ]
    where opl = if nums !! (length ops + 1) == 0 then [(+), (-), (*)] else oplist

goal :: Node -> Bool
goal (ops, nums, v') = length ops == length nums - 1
    && eval' nums ops (length ops - 1) == v'

yield_gtf :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_gtf array result = map toArray (yield_gtf' array')
    where len = (rangeSize $ bounds array) - 1
          array' = elems array
          yield_gtf' = (filt result) . (transform array') . (generate len)
          toArray l = listArray (0, (length (snd l) - 1)) (snd l)

generate :: Int -> [Int] -> [[Int -> Int -> Int]]
generate 0 _ = [[]]
generate len nums = [ op : tail'
                    | tail' <- generate (len - 1) nums
                    , op <- opl tail'
                    ]
    where opl t = if nums !! (length t + 1) == 0 then [(+), (-), (*)] else oplist

transform :: [Int] -> [[Int -> Int -> Int]] -> [(Int, [Int -> Int -> Int])]
transform nums ops = map (\x -> (eval' nums x ((length x) - 1), x)) ops

filt :: Int -> [(Int, [Int -> Int -> Int])] -> [(Int, [Int -> Int -> Int])]
filt result ops = filter (\(result', _) -> result' == result) ops

(./.) :: Int -> Int -> Int
(./.) = div

instance Show (Int -> Int -> Int) where
    show f | f 10 5 == 15 = "plus"
           | f 10 5 == 5 = "minus"
           | f 10 5 == 50 = "times"
           | f 10 5 == 2 = "div"
           | otherwise = error "undefined operator"

{- ----------------------------------------------------------------------------
 - searchDfs plus all needed abstract datatypes.
 - ---------------------------------------------------------------------------}

searchDfs :: (node -> [node]) -> (node -> Bool)
                          -> node -> [node]
searchDfs succ goal x = (search' (push x emptyStack) )
 where
   search' s
    | (stackEmpty s)   = []
    | goal (top s)     = (top s):(search' (pop s))
    | otherwise        = let x = top s
                         in search' (foldr push (pop s) (succ x))

emptyStack :: Stack a
stackEmpty :: Stack a -> Bool
push       :: a -> Stack a -> Stack a
pop        :: Stack a -> Stack a
top        :: Stack a -> a

type Stack a  = [a]

emptyStack    = []

stackEmpty [] = True
stackEmpty _  = False

push x xs     = x:xs

pop (_:xs)    = xs
top (x:_)     = x
