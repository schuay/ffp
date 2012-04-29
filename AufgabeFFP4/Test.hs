module Main where

import AufgabeFFP4
import Test.HUnit

knapsackT1 = TestCase $ assertEqual
    "Empty list must result in another one"
    ([], 0)
    (knapsack [] 5)

knapsackT2 = TestCase $ assertEqual
    "Load with maximum value should be selected"
    ([(1,1),(5,8)], 9)
    (knapsack [(7,9), (6,8), (5,8), (1,1), (8,10), (5,2)] 6)

knapsackT3 = TestCase $ assertEqual
    "Test case from task description"
    ([(3,4),(3,4),(2,3),(2,3)], 14)
    (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10)

knapsackT4 = TestCase $ assertEqual
    "Load with maximum value should be selected - no solution"
    ([], 0)
    (knapsack [(5,3),(2,7),(2,6),(10,100)] 1)

knapsackT5 = TestCase $ assertEqual
    "Load with maximum value should be selected"
    ([(2,6),(2,7)], 13)
    (knapsack [(5,3),(2,7),(2,6),(10,100)] 5)

knapsackT6 = TestCase $ assertEqual
    "Load with maximum value should be selected"
    ([(10,100),(2,7)], 107)
    (knapsack [(5,3),(2,7),(2,6),(10,100)] 13)

knapsackGroup = TestList [ knapsackT1, knapsackT2, knapsackT3, knapsackT4,
                           knapsackT5, knapsackT6 ]

binomDynK0 = TestCase $ assertEqual
    "Result is 1 if k==0"
    1
    (binomDyn (7,0))

binomDynNK = TestCase $ assertEqual
    "Result is 1 if n==k"
    1
    (binomDyn (5,5))

binomDynInv1 = TestCase $ assertEqual
    "Result is 0 if n < k"
    0
    (binomDyn (5,6))

binomDynInv2 = TestCase $ assertEqual
    "Result is 0 if n < 0"
    0
    (binomDyn (-1,6))

binomDynInv3 = TestCase $ assertEqual
    "Result is 0 if k < 0"
    0
    (binomDyn (1,-1))

binomDynT1 = TestCase $ assertEqual
    "Result should equal reference function"
    (binom (8,5))
    (binomDyn (8,5))

binomDynT2 = TestCase $ assertEqual
    "Result should equal reference function"
    (binom (4,2))
    (binomDyn (4,2))

binomDynT3 = TestCase $ assertEqual
    "Result should equal reference value"
    126410606437752
    (binomDyn (50,25))

binomGroup = TestList [ binomDynK0, binomDynNK, binomDynInv1, binomDynInv2,
                        binomDynInv3, binomDynT1, binomDynT2, binomDynT3 ]

main = runTestTT $ TestList [ knapsackGroup, binomGroup ]
