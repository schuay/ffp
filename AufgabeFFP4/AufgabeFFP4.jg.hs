module AufgabeFFP4 where

import Data.List (delete)

type NodeKnp = (Value, Weight, MaxWeight, [Object], SolKnp)
type Weight = Int
type Value = Int
type MaxWeight = Weight
type Object = (Weight, Value)
type Objects = [Object]
type SolKnp = [Object]

{- Backpack problem.
 - Given a set of objects (consisting of a value and a weight), find a
 - selection of objects such that the sum of values is maximal without
 - exceeding a certain weight limit.  If there are several best solutions, any
 - of them are valid.
 -
 - knapsack must use the functional searchDfs and solve the problem using
 - backtracking.
 -
 - Use a tree with nodes consisting of: 
 -  current weight
 -  current value
 -  maximum weight
 -  selected objects
 -  not selected but valid objects -}


{- Successor nodes are constructed by adding an element of objects to
 - the current backpack selection, as long as their weight isn't higher than
 - the given limit. -}
succKnp :: NodeKnp -> [NodeKnp]
succKnp (v, w, limit, objects, psol)
    = [ (v + v', w + w', limit, delete o objects, o:psol)
        | o@(w', v') <- objects, w + w' <= limit ]

{- A goal node is reached once no more objects can be added without
 - exceeding the given weight limit. -}
goalKnp :: NodeKnp -> Bool
goalKnp (_, w, limit, objects, _) = and [ w + w' > limit | (w', _) <- objects ]

{- Returns best solution from the set of all solutions produced by
 - searchDfs. -}
knapsack :: Objects -> MaxWeight -> (SolKnp,Value)
knapsack objects limit
    | solutions == [] = ([], 0)
    | otherwise = (obj, v)
    where
    solutions = searchDfs succKnp goalKnp (0, 0, limit, objects, [])
    values = map (\(v, _, _, _, _) -> v) solutions
    (v, _, _, _, obj) = head $ filter (\(v', _, _, _, _) -> v' == maximum values) solutions

{- Implement binomial coefficient calculation using dynamic programming.  Use
 - the functional dynamic and functions compB and bndsB. -}

{- dynamic :: ...
 - dynamic = ...
 -
 - compB :: ...
 - bndsB :: ...
 -}

{- Binomial coefficients, naive implementation. -}

binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)

binomDyn :: (Integer,Integer) -> Integer
binomDyn (m,n) = undefined

{- ----------------------------------------------------------------------------
 - searchDfs, dynamic plus all needed abstract datatypes.
 - ---------------------------------------------------------------------------}

searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool)
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
