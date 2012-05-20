module AufgabeFFP7 where

import Test.QuickCheck

-- 1 --

type Buffer = (Int,String)
type BufferI = (String,String)

{- Helper to fix a cursor position outside of the buffer -}
fixI :: Buffer -> Int
fixI (i, s)
  | i < 0 = 0
  | i > length s = (length s)
  | otherwise = i

empty :: Buffer
empty = (0, [])

emptyI :: BufferI
emptyI = ([], [])

prop_empty = retrieve emptyI == empty

insert :: Char -> Buffer -> Buffer
insert c (i, s) = (i'+1, s')
  where s' = (take i' s) ++ c:(drop i' s)
        i' = fixI (i, s)

insertI :: Char -> BufferI -> BufferI
insertI c (b, a) = (c:b, a)

prop_insert c x = retrieve (insertI c x) == insert c (retrieve x)

delete :: Buffer -> Buffer
delete (i, s) = (i', s')
  where i' = fixI (i-1, s)
        s' = (take (i-1) s) ++ (drop i s)

deleteI :: BufferI -> BufferI
deleteI ([], a) = ([], a)
deleteI (c:b, a) = (b, a)

prop_delete x = retrieve (deleteI x) == delete (retrieve x)

left :: Buffer -> Buffer
left (i, s) = (i', s)
  where i' = fixI (i-1, s)

leftI :: BufferI -> BufferI
leftI ([], a) = ([], a)
leftI (c:b, a) = (b, c:a)

prop_left x = retrieve (leftI x) == left (retrieve x)

right :: Buffer -> Buffer
right (i, s) = (i', s)
  where i' = fixI (i+1, s)

rightI :: BufferI -> BufferI
rightI (b, []) = (b, [])
rightI (b, c:a) = (c:b, a)

prop_right x = retrieve (rightI x) == right (retrieve x)

atLeft :: Buffer -> Bool
atLeft (i, _) = i <= 0

atLeftI :: BufferI -> Bool
atLeftI ([], _) = True
atLeftI (_, _) = False

prop_atLeft x = (atLeftI x) == atLeft (retrieve x)

atRight :: Buffer -> Bool
atRight (i, s) = i >= len
  where len = (length s)

atRightI :: BufferI -> Bool
atRightI (_, []) = True
atRightI (_, _) = False

prop_atRight x = (atRightI x) == atRight (retrieve x)

retrieve :: BufferI -> Buffer
retrieve (b, a) = (length b, (reverse b) ++ a)

-- 2 --

--FIXME: Is this really an [Int]? In the script it is just Int.
type Nat = [Int]

{- The ssfn from the lecture script. -}
ssfn :: [Integer] -> Integer
ssfn = (sap 0) . removeDuplicates . quickSort

sap :: Integer -> [Integer] -> Integer
--sap n [] = 0
sap n [] = n -- FIXME: dirty hack to make this code solve the test cases
sap n (x:xs)
  | n /= x = n
  | otherwise = sap (n+1) xs

quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (x:xs) = quickSort [ y | y<-xs, y<=x ] ++ [x]
                   ++ quickSort [ y | y<-xs, y>x ]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:removeDuplicates (xs \\ [x])

{- The minfree from the lecture script. -}
-- FIXME: Currently, we assume that this returns an in (not another Nat
-- assume we get an Int and not another Nat.
minfree :: Nat -> Int
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = filter (`notElem` ys) xs

prop_ssfn_eq_minfree_a :: Nat -> Bool
prop_ssfn_eq_minfree_a x = or (map (<0) x) || ssfn [ fromIntegral a | a<-x ]
                           == fromIntegral (minfree x)

{- The list may only contain natural numbers. -}
invariant_nat_nums :: Nat -> Bool
invariant_nat_nums x = and (map (>=0) x)

prop_ssfn_eq_minfree_b :: Nat -> Property
prop_ssfn_eq_minfree_b x = invariant_nat_nums x ==>
                           ssfn [ fromIntegral a | a<-x ]
			   == fromIntegral (minfree x)
