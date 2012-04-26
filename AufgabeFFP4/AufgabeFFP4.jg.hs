module AufgabeFFP4 where

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

{- searchDfs :: ...
 - searchDfs =
 -}

succKnp :: NodeKnp -> [NodeKnp]
succKnp (v, w, limit, objects, psol) = undefined

goalKnp :: NodeKnp -> Bool
goalKnp (_, w, limit, ((w', _):_), _) = undefined

knapsack :: Objects -> MaxWeight -> (SolKnp,Value)
knapsack objects limit = undefined

{- Implement binomial coefficient calculation using dynamic programming.  Use
 - the functional dynamic and functions compB and bndsB. -}

{- dynamic :: ...
 - dynamic = ...
 -
 - compB :: ...
 - bndsB :: ...
 -}

{-- Binomial coefficients, naive implementation. --}

binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)

binomDyn :: (Integer,Integer) -> Integer
binomDyn (m,n) = undefined
