module AufgabeFFP8 where

import Test.QuickCheck
import Data.List ((\\), nub)

type Nat = Int

{- minfree_bv -}

minfree_bv :: [Nat] -> Nat
minfree_bv xs = head ([0..] \\ xs)

minfree_chl = undefined
minfree_col = undefined
minfree_b = undefined
minfree_r = undefined
minfree_o = undefined
minfree_bhof = undefined
minfree_rhof = undefined
minfree_ohof = undefined

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
