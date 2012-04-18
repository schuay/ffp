module Main where

import AufgabeFFP3
import Test.HUnit

import Prelude hiding (filter)
import Data.List (sort)

generatorEmpty = TestCase $ assertEqual
    "If there are no items there is no way to pack them"
    []
    (generator [])

generatorT1 = TestCase $ assertEqual
    "All possible combinations of the given items must be formed"
    [[(2,6)],[(2,6),(10,100)],[(2,7)],[(2,7),(2,6)],[(2,7),(2,6),(10,100)],
    [(2,7),(10,100)],[(5,13)],[(5,13),(2,6)],[(5,13),(2,6),(10,100)],
    [(5,13),(2,7)],[(5,13),(2,7),(2,6)],[(5,13),(2,7),(2,6),(10,100)],
    [(5,13),(2,7),(10,100)],[(5,13),(10,100)],[(10,100)]]
    ((sort . generator) [(5,13),(2,7),(2,6),(10,100)])

transformerEmpty = TestCase $ assertEqual
    "Empty list must result in another one"
    []
    (transformer [])

transformerT1 = TestCase $ assertEqual
    "Weight and value have to be added correctly"
    [([(5,13),(2,7),(10,100)],17,120),([(5,13),(10,100)],15,113),([(10,100)],10,100)]
    ((sort . transformer) [[(5,13),(2,7),(10,100)],[(5,13),(10,100)],[(10,100)]])

filterEmpty = TestCase $ assertEqual
    "Empty list must result in another one"
    []
    (filter [] 5)

filterT1 = TestCase $ assertEqual
    "All loads with weight > 15 should be removed"
    [([(5,13),(10,100)],15,113),([(10,100)],10,100)]
    (sort (filter [([(5,13),(2,7),(10,100)],17,120),([(5,13),(10,100)],15,113),
                      ([(10,100)],10,100)] 15))

selector1Empty = TestCase $ assertEqual
    "Empty list must result in another one"
    []
    (selector1 [])

selector1T1 = TestCase $ assertEqual
    "All loads which maximize value should be selected"
    [([(5,8),(1,1)],6,9),([(6,9)],6,9),([(8,9)],8,9)]
    ((sort . selector1) [([(6,9)],6,9),([(6,8)],6,8),([(5,8),(1,1)],6,9),([(8,9)],8,9),
                        ([(5,2)],5,2)])

selector2Empty = TestCase $ assertEqual
    "Empty list must result in another one"
    []
    (selector2 [])

selector2T1 = TestCase $ assertEqual
    "All loads which maximize value should be selected"
    [([(5,8),(1,1)],6,9),([(6,9)],6,9)]
    ((sort . selector2) [([(6,9)],6,9),([(6,8)],6,8),([(5,8),(1,1)],6,9),([(8,9)],8,9),
                        ([(5,2)],5,2)])

combinedT1 = TestCase $ assertEqual
    "Test case from Task description"
    [([(2,7),(2,6)],4,13)]
    ((sort . selector1) ((filter . transformer . generator) [(5,3),(2,7),(2,6),(10,100)] 5))

combinedT2 = TestCase $ assertEqual
    "Test case from Task description"
    [([(2,7),(10,100)],12,107)]
    ((sort . selector1) ((filter . transformer . generator) [(5,3),(2,7),(2,6),(10,100)] 13))

combinedT3 = TestCase $ assertEqual
    "Test case from Task description"
    []
    ((sort . selector1) ((filter . transformer . generator) [(5,3),(2,7),(2,6),(10,100)] 1))

combinedT4 = TestCase $ assertEqual
    "Test case from Task description"
    [([(2,7),(2,6)],4,13),([(5,13)],5,13)]
    ((sort . selector1) ((filter . transformer . generator) [(5,13),(2,7),(2,6),(10,100)] 5))

combinedT5 = TestCase $ assertEqual
    "Test case from Task description"
    [([(2,7),(2,6)],4,13)]
    ((sort . selector2) ((filter . transformer . generator) [(5,13),(2,7),(2,6),(10,100)] 5))

knapsackGroup = TestList [ generatorEmpty, generatorT1, transformerEmpty, transformerT1,
                           filterEmpty, filterT1, selector1Empty, selector1T1,
                           selector2Empty, selector2T1, combinedT1, combinedT2, combinedT3,
                           combinedT4, combinedT5 ]

binomSK0 = TestCase $ assertEqual
    "Result is 1 if k==0"
    1
    (binomS (7,0))

binomSNK = TestCase $ assertEqual
    "Result is 1 if n==k"
    1
    (binomS (5,5))

binomSInv1 = TestCase $ assertEqual
    "Result is 0 if n < k"
    0
    (binomS (5,6))

binomSInv2 = TestCase $ assertEqual
    "Result is 0 if n < 0"
    0
    (binomS (-1,6))

binomSInv3 = TestCase $ assertEqual
    "Result is 0 if k < 0"
    0
    (binomS (1,-1))

binomST1 = TestCase $ assertEqual
    "Result should equal reference function"
    (binom (8,5))
    (binomS (8,5))

binomST2 = TestCase $ assertEqual
    "Result should equal reference function"
    (binom (4,2))
    (binomS (4,2))

binomST3 = TestCase $ assertEqual
    "Result should equal reference value"
    126410606437752
    (binomS (50,25))

binomMK0 = TestCase $ assertEqual
    "Result is 1 if k==0"
    1
    (binomM (7,0))

binomMNK = TestCase $ assertEqual
    "Result is 1 if n==k"
    1
    (binomM (5,5))

binomMInv1 = TestCase $ assertEqual
    "Result is 0 if n < k"
    0
    (binomM (5,6))

binomMInv2 = TestCase $ assertEqual
    "Result is 0 if n < 0"
    0
    (binomM (-1,6))

binomMInv3 = TestCase $ assertEqual
    "Result is 0 if k < 0"
    0
    (binomM (1,-1))

binomMT1 = TestCase $ assertEqual
    "Result should equal reference function"
    (binom (8,5))
    (binomM (8,5))

binomMT2 = TestCase $ assertEqual
    "Result should equal reference function"
    (binom (4,2))
    (binomM (4,2))

binomMT3 = TestCase $ assertEqual
    "Result should equal reference value"
    126410606437752
    (binomM (50,25))

binomGroup = TestList [ binomSK0, binomSNK, binomSInv1, binomSInv2,
                        binomSInv3, binomST1, binomST2, binomST3, binomMK0,
                        binomMNK, binomMInv1, binomMInv2,
                        binomMInv3, binomMT1, binomMT2, binomMT3 ]

main = runTestTT $ TestList [ knapsackGroup, binomGroup ]
