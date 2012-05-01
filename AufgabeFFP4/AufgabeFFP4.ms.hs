module AufgabeFFP4 where

import Data.Ix
import Data.List
import Data.Array

-- 1 --

type NodeKnp = (Value, Weight, MaxWeight, [Object], SolKnp)

type Weight = Int
type Value = Int
type MaxWeight = Weight
type Object = (Weight, Value)
type Objects = [Object]
type SolKnp = [Object]

data Stack a = EmptyStk | Stk a (Stack a)

push x s = Stk x s

pop EmptyStk = error "pop from an empty stack"
pop (Stk _ s) = s

top EmptyStk = error "top from an empty stack"
top (Stk x _) = x

emptyStack = EmptyStk

stackEmpty EmptyStk = True
stackEmpty _ = False

searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = (search' (push x emptyStack) )
  where search' s
          | stackEmpty s = []
          | goal (top s) = top s : search' (pop s)
          | otherwise = let x = top s
                        in search' (foldr push (pop s) (succ x))

{- Generates all successors of the given node. A successor is a node where
 - one additional item has been added and the weight is less than limit. -}
succKnp :: NodeKnp -> [NodeKnp]
succKnp (v,w,limit,objects,psol) = neigh
  where nextObj x = delete x objects
        next x = (v+(snd x), w+(fst x), limit, nextObj x, x:psol)
        neigh = [ next x | x<-objects, w+(fst x) <= limit ]

{- Returns true if the given node is a solution (no item can be added
 - without overtaxing the knapsack) -}
goalKnp :: NodeKnp -> Bool
goalKnp (_,w,limit,rem,_) = and [ w+(fst x) > limit | x<-rem ]

{- Returns the first solution to the knapsack problem with maximum value. -}
knapsack :: Objects -> MaxWeight -> (SolKnp,Value)
knapsack objects limit = (sol,val)
  where sols = searchDfs succKnp goalKnp (0,0,limit,objects,[])
  	(val,_,_,_,sol) = maximumBy (ord) sols
  	ord (v,_,_,_,_) (v',_,_,_,_)
	  | v > v'  = GT
	  | v == v' = EQ
	  | v < v'  = LT

-- 2 --

newtype Table b a = Tbl (Array a b)

newTable l = Tbl (array (lo,hi) l)
  where indices = map fst l
        lo = minimum indices
        hi = maximum indices

findTable (Tbl a) i = a ! i

updTable p@(i,x) (Tbl a) = Tbl (a // [p])

dynamic :: (Ix coord) => (Table entry coord -> coord -> entry) -> (coord,coord)
                         -> (Table entry coord)
dynamic compute bnds = t
  where t = newTable (map (\coord -> (coord,compute t coord)) (range bnds))

{- Each table entry contains a list of all binom(i,k) where i is the table
 - index and k is element of [0..i] -}
compB :: Table [Integer] Integer -> Integer -> [Integer]
compB t i
  | i == 0 = [0]
  | otherwise = [1] ++ [(bin . fromIntegral) k | k<- [1..(i-1)]] ++ [1]
    where bin k = (findTable t (i-1))!!(k-1) + (findTable t (i-1))!!k

{- The size of our table. Should be n. -}
bndsB :: Integer -> (Integer,Integer)
bndsB n = (0,n)

{- Calculate the binomial coefficient using dynamic programming.
 - Should produce the same results as binom but is much faster. -}
binomDyn :: (Integer,Integer) -> Integer
binomDyn (n,k)
  | k < 0 || n < 0 || k > n = 0
  | otherwise               = (findTable t n)!!k'
  where t = dynamic compB (bndsB n)
        k' = fromIntegral k

{- Reference implementation for calculating the binomial coefficient
 - (slow, not handling all input). -}
binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)
