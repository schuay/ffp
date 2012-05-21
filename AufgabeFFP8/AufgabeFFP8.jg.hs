module AufgabeFFP8 where

import Test.QuickCheck
import Data.List ((\\), nub)
import Data.Array

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

minfree_col = undefined
minfree_b = undefined
minfree_r = undefined
minfree_o = undefined
minfree_bhof = undefined
minfree_rhof = undefined
minfree_ohof = undefined

b_indiv :: p -> Bool
b_indiv = undefined

b_solve :: p -> s
b_solve = undefined

b_divide :: p -> [p]
b_divide = undefined

b_combine :: p -> [s] -> s
b_combine = undefined

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
prop_allImplsEq_b x = invariant_no_dupes x ==> 
                      all (== reference) results
    where x' = map abs x
          reference = minfree_bv x'
          results = [ f x' | f <- minfree_fns ]
