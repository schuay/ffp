module AufgabeFFP3 where
--import Data.List (subsequences) -- cool but not available in hugs :(
import Data.List (nub)
import qualified Prelude (filter)
import Prelude hiding (filter)

-- 1 --
type Weight = Int
type Value = Int
type Item = (Weight, Value)
type Items = [Item]
type Load = [Item]
type Loads = [Load]
type LoadWghtVal = (Load, Weight, Value)
type MaxWeight = Weight

{- Generates a list of all selections of elements in i. -}
generator :: Items -> Loads
--generator = (drop 1 . subsequences)
generator [] = []
generator i = i : rest
  where rest = (nub . concat . map (\x -> generator [ a | a<-i, a/=x ])) i

{- Adds up weights and values of all elements in lds and returns
 - (lds, sum(weights), sum(values)). -}
transformer :: Loads -> [LoadWghtVal]
transformer lds = [ (l, weight l, value l) | l <- lds ]
  where weight l = (sum . map (\x -> fst x)) l
        value l = (sum . map (\x -> snd x)) l

{- Returns all (Load, Weight, Value) touples with Weight <= mw. -}
filter :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
filter mw lwv = Prelude.filter (\(_, x, _) -> x <= mw) lwv

{- Returns all LoadWghtVals with the maximum value (may be more than one). -}
selector1:: [LoadWghtVal] -> [LoadWghtVal]
selector1 lwv = [ (l, w, v) | (l, w, v) <- lwv, v == maxVal lwv ]
  where maxVal = (maximum . map (\(_, _, x) -> x))

{- Returns all LoadWghtVals with the maximum value and the minimum weight
 - (may be more than one). -}
selector2 :: [LoadWghtVal] -> [LoadWghtVal]
selector2 lwv = [ (l, w, v) | (l, w, v) <- maxVals, w == minWeight maxVals ]
  where minWeight = (minimum . map (\(_, x, _) -> x))
        maxVals = selector1 lwv

-- 2 --

{- Reference implementation for calculating the binomial coefficient
 - (slow, not handling all input). -}
binom :: (Integer, Integer) -> Integer
binom (n,k)
    | k == 0 || n == k = 1
    | otherwise = binom (n - 1, k - 1) + binom (n - 1, k)

{- Generates the rows of the pascal triangle. This can be used to calculate
 - the binomial coefficient (the row is n, the column is k).
 - The list is infinite, the nth row contains n+1 elements -}
pascal :: [[Integer]]
pascal = [1] : map (\x -> [1] ++ (next x) ++ [1]) pascal
  where next :: [Integer] -> [Integer]
        next x = zipWith (+) x (tail x)

{- Calculate the binomial coefficient using the pascal triangle.
 - Should produce the same results as binom but is much faster. -}
binomS :: (Integer,Integer) -> Integer
binomS (n,k)
  | k < 0 || n < 0 || k > n  = 0
  | k == 0 || n == k         = 1
  | otherwise                = ((pascal!!(n'-1))!!(k'-1))+((pascal!!(n'-1))!!k')
    where n' = fromIntegral n
          k' = fromIntegral k

{- A stream containing all rows of the pascal triangle, but uses binomM
 - to calculate all elements and is in turn used by binomM to get results. -}
memoBinom :: [[Integer]]
memoBinom = [ [ binomM (n',k') | k'<-[0..n'] ] | n'<-[0..] ]

{- Calculate the binomial coefficient using memoBinom.
 - Should produce the same results as binom but is much faster. -}
binomM :: (Integer,Integer) -> Integer
binomM (n,k)
  | k < 0 || n < 0 || k > n  = 0
  | k == 0 || n == k         = 1
  | otherwise = (memoBinom!!(n'-1))!!(k'-1) + (memoBinom!!(n'-1))!!k'
    where n' = fromIntegral n
          k' = fromIntegral k
