module Main where

import AufgabeFFP6
import Test.HUnit
import Data.Array


testEval1 = TestCase $ assertEqual
    "Standard case"
    (5)
    (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(*)),(2,(+))]))

testEval2 = TestCase $ assertEqual
    "Standard case"
    (-3)
    (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(-)),(2,(*))]))

testEval3 = TestCase $ assertEqual
    "Standard case"
    (0)
    (eval (array (1,3) [(1,1),(2,2),(3,3)]) (array (1,2) [(1,(+)),(2,(-))]))

arr :: Array Int Int
arr = array (1,3) [(1,1),(2,2),(3,3)]

testYieldBT1 = TestCase $ assertEqual
    "Standard case"
    ([6, 6])
    (map (eval arr) $ yield_bt arr 6)

testYieldBT2 = TestCase $ assertEqual
    "No results"
    ([])
    (map (eval arr) $ yield_bt arr 4)

testYieldBT3 = TestCase $ assertEqual
    "Standard case"
    ([0, 0, 0, 0])
    (map (eval arr) $ yield_bt arr 0)

testYieldGTF1 = TestCase $ assertEqual
    "Standard case"
    ([6, 6])
    (map (eval arr) $ yield_gtf arr 6)

testYieldGTF2 = TestCase $ assertEqual
    "No results"
    ([])
    (map (eval arr) $ yield_gtf arr 4)

testYieldGTF3 = TestCase $ assertEqual
    "Standard case"
    ([0, 0, 0, 0])
    (map (eval arr) $ yield_gtf arr 0)

testShow1 = TestCase $ assertEqual
    "Standard case"
    ("array (1,2) [(1,plus),(2,minus)]")
    (show $ (array (1,2) [(1,(+)),(2,(-))] :: Array Int (Int -> Int -> Int)))
    
testShow2 = TestCase $ assertEqual
    "Standard case"
    ("array (1,2) [(1,div),(2,times)]")
    (show $ (array (1,2) [(1,div),(2,(*))] :: Array Int (Int -> Int -> Int)))


groupEval = TestList [ testEval1, testEval2, testEval3 ]
groupYield = TestList [ testYieldBT1, testYieldBT2, testYieldBT3
                      , testYieldGTF1, testYieldGTF2, testYieldGTF3
                      ]
groupShow = TestList [ testShow1, testShow2 ]

main = runTestTT $ TestList [ groupEval, groupYield, groupShow ]
