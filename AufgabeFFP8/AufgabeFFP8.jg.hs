module AufgabeFFP8 where

import Test.QuickCheck
import Data.List ((\\), nub, partition)
import Data.Array
import Data.Maybe

type Nat = Int

{- minfree_bv -}

minfree_bv :: [Nat] -> Nat
minfree_bv xs = head ([0..] \\ xs)

{- minfree_chl -}

minfree_chl :: [Nat] -> Nat
minfree_chl = search . checklist
    where search :: Array Int Bool -> Int
          search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n)
               (zip (filter (<= n) xs) (repeat True))
    where n = length xs

{- minfree_col -}

minfree_col :: [Nat] -> Nat
minfree_col = search . countlist
    where search :: Array Int Int -> Int
          search = length . takeWhile (/= 0) . elems

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
    where n = if null xs then 0 else  maximum xs

{- minfree_b
 - 
 - The implementation from the slides is incomplete (the definition of
 - b is missing). Also, I don't see much of a divide and conquer approach here. -}

minfree_b :: [Nat] -> Nat
minfree_b xs = if null ([0..b - 1] \\ us)
               then head ([b..] \\ vs)
               else head ([0..] \\ us)
    where (us, vs) = partition (< b) xs
          b = (length xs) `div` 2

{- minfree_r -}

minfree_r :: [Nat] -> Nat
minfree_r xs
    | nub xs == xs = minfrom_r 0 xs
    | otherwise = 0 {- Avoid infinite loops. -}

minfrom_r a xs | null xs = a
               | length us == b - a = minfrom_r b vs
               | otherwise = minfrom_r a us
    where (us, vs) = partition (< b) xs
          n = length xs
          b = a + 1 + n `div` 2

{- minfree_o -}

minfree_o :: [Nat] -> Nat
minfree_o xs
    | nub xs == xs = minfrom_o 0 (length xs, xs)
    | otherwise = 0 {- Avoid infinite loops. -}

minfrom_o a (n, xs) | n == 0 = a
                    | m == b - a = minfrom_o b (n - m, vs)
                    | otherwise = minfrom_o a (m, us)
    where (us, vs) = partition (< b) xs
          b = a + 1 + n `div` 2
          m = length us

{- minfree_bhof -}

{- A problem consists of a list of numbers, the beginning and end indices
 - and whether a split has occurred. -}
type ProblemB = ([Nat], Int, Int, Bool)

minfree_bhof :: [Nat] -> Nat
minfree_bhof xs
    | nub xs == xs = fromMaybe (length xs) dac
    | otherwise = 0 {- Avoid infinite loops. -}
    where dac = divideAndConquer b_indiv b_solve b_divide b_combine (xs, 0, length xs - 1, False)

b_indiv :: ProblemB -> Bool
b_indiv (_, _, _, b) = b

b_solve :: ProblemB -> Maybe Int
b_solve (xs, i, j, _) | null first = Nothing
                      | otherwise = Just $ head first
    where first = [i..j] \\ xs

b_divide :: ProblemB -> [ProblemB]
b_divide (xs, i, j, _) = [ (us, i, b - 1, True), (vs, b, j, True) ]
    where (us, vs) = partition (< b) xs
          n = length xs
          b = if i + n `div` 2 == 0 then 1 else i + n `div` 2

b_combine :: ProblemB -> [Maybe Int] -> Maybe Int
b_combine _ xs
    | null xs' = Nothing
    | otherwise = Just (minimum xs')
    where xs' = catMaybes xs

{- minfree_rhof -}

{- A problem consists of a list of numbers, and the beginning and end indices
 - of the current subproblem. -}
type ProblemR = ([Nat], Int, Int)

minfree_rhof :: [Nat] -> Nat
minfree_rhof xs
    | nub xs == xs = fromMaybe (length xs) dac
    | otherwise = 0 {- Avoid infinite loops. -}
    where dac = divideAndConquer r_indiv r_solve r_divide r_combine (xs, 0, length xs - 1)

r_indiv :: ProblemR -> Bool
r_indiv (_, a, b) = b - a + 1 <= 1

r_solve :: ProblemR -> Maybe Int
r_solve ([], a, _) = Just a
r_solve (x:xs, a, _) | x == a = Nothing
                     | otherwise = Just a

r_divide :: ProblemR -> [ProblemR]
r_divide (xs, a, c) = [ (us, a, b - 1), (vs, b, c) ]
    where (us, vs) = partition (< b) xs
          n = c - a + 1
          b = a + (n + 1) `div` 2

r_combine :: ProblemR -> [Maybe Int] -> Maybe Int
r_combine _ xs
    | null xs' = Nothing
    | otherwise = Just (minimum xs')
    where xs' = catMaybes xs

{- minfree_ohof -}

minfree_ohof :: [Nat] -> Nat
minfree_ohof xs
    | nub xs == xs = fromMaybe n dac
    | otherwise = 0 {- Avoid infinite loops. -}
    where dac = divideAndConquer o_indiv o_solve o_divide o_combine (xs, 0, n - 1)
          n = length xs

o_indiv :: ProblemR -> Bool
o_indiv = r_indiv

o_solve :: ProblemR -> Maybe Int
o_solve = r_solve

o_divide :: ProblemR -> [ProblemR]
o_divide = r_divide

o_combine :: ProblemR -> [Maybe Int] -> Maybe Int
o_combine = r_combine

{- The divide and conquer function from
 - http://www.iro.umontreal.ca/~lapalme/AlgoFP/ -}
divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer ind solve divide combine initPb = dc' initPb
    where dc' pb | ind pb  = solve pb
                 | otherwise = combine pb (map dc' (divide pb))

{- TODO: The specification is (surprise) inconsistent
 - in reference to the required quickcheck tests.
 -
 - It mentions testing nonduplicate lists (these should never fail),
 - and duplicate lists (these can differ), but then goes on
 - to describe the same properties as FFP7. For now,
 - assume that prop_allImplsEq_a tests all lists, and prop_allImplsEq_b
 - tests lists without duplicates. -}

minfree_fns = [ minfree_bv, minfree_chl, minfree_col
              , minfree_b, minfree_r, minfree_o
              , minfree_bhof, minfree_rhof, minfree_ohof
              ]

prop_allImplsEq_a :: [Nat] -> Bool
prop_allImplsEq_a x = all (== reference) results
    where x' = map abs x
          reference = minfree_bv x'
          results = [ f x' | f <- minfree_fns ]

invariant_no_dupes :: [Nat] -> Bool
invariant_no_dupes x = nub x == x

prop_allImplsEq_b :: [Nat] -> Property
prop_allImplsEq_b x = invariant_no_dupes x' ==>
                      all (== reference) results
    where x' = map abs x
          reference = minfree_bv x'
          results = [ f x' | f <- minfree_fns ]
