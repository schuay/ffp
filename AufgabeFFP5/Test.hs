module Main where

import AufgabeFFP5
import Test.HUnit
import Data.Array

a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),(5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)]
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),(5,2),(6,(-1)),(7,2),(8,0),(9,(-1))]
c = array (1,5) [(1,2),(2,3),(3,(-10)),(4,1),(5,4)]

data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq,Ord,Ix,Show)
d = array (Tue,Sat) [(Wed,"work"),(Thu,"study"),(Tue,"study"),(Fri,"chill"),(Sat,"relax")]


testMas1 = TestCase $ assertEqual
    "mas should return highest partial sum"
    (12)
    (mas a)

testMas2 = TestCase $ assertEqual
    "mas should return highest partial sum"
    (12)
    (mas b)

testAmas1 = TestCase $ assertEqual
    "amas should return all index ranges with highest partial sum"
    ([(3,7),(4,7)])
    (amas a)

testAmas2 = TestCase $ assertEqual
    "amas should return all index ranges with highest partial sum"
    ([(1,7),(1,8),(4,7),(4,8)])
    (amas b)

testAmas3 = TestCase $ assertEqual
    "lmas should return longest index range with highest partial sum"
    (3, 7)
    (lmas a)

testLmas1 = TestCase $ assertEqual
    "lmas should return longest index range with highest partial sum"
    (1, 8)
    (lmas b)

testLmas2 = TestCase $ assertEqual
    "lmas should return longest index range with highest partial sum"
    (1, 2)
    (lmas c)

testMinIndex1 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (4)
    (minIndex a (>5))

testMinIndex2 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (2)
    (minIndex a (<0))

testMinIndex3 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (3)
    (minIndex a (even))

testMinIndex4 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (1)
    (minIndex b (odd))

testMinIndex5 = TestCase $ assertEqual
    "minIndex should result in error if no element matches"
    (error "No matching index") {-- This probably won't work. --}
    (minIndex b (>100))

testMinIndex6 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (Sat)
    (minIndex d (=="relax"))

testMinIndex7 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (Wed)
    (minIndex d (=="work"))

testMinIndex8 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (Fri)
    (minIndex d (=="chill"))

testMinIndex9 = TestCase $ assertEqual
    "minIndex should return first index for which f evaluates to True"
    (Tue)
    (minIndex d (/="chill"))

testMinIndex10 = TestCase $ assertEqual
    "minIndex should result in error if no element matches"
    (error "No matching index") {-- This probably won't work. --}
    (minIndex d (=="swim"))

groupMas = TestList [ testMas1, testMas2 ]
groupAmas = TestList [ testAmas1, testAmas2, testAmas3 ]
groupLmas = TestList [ testLmas1, testLmas2 ]
groupMinIndex = TestList [ testMinIndex1, testMinIndex2, testMinIndex3,
                           testMinIndex4, testMinIndex5, testMinIndex6,
                           testMinIndex7, testMinIndex8, testMinIndex9,
                           testMinIndex10 ]

main = runTestTT $ TestList [ groupMas, groupAmas, groupLmas, groupMinIndex ]
