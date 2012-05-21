module AufgabeFFP8 where

import Test.QuickCheck
import Data.List ((\\))

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

{-
prop_ssfn_eq_minfree_a :: [Nat] -> Bool
prop_ssfn_eq_minfree_a x = ssfn [ fromIntegral a | a<-x ]
                           == fromIntegral (minfree x)

invariant_nat_nums :: [Nat] -> Bool
invariant_nat_nums x = and (map (>=0) x)

prop_ssfn_eq_minfree_b :: [Nat] -> Property
prop_ssfn_eq_minfree_b x = invariant_nat_nums x ==>
                           ssfn [ fromIntegral a | a<-x ]
			   == fromIntegral (minfree x)
-}
