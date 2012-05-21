module Main where

import AufgabeFFP8
import Test.HUnit

tests = [ ("Empty list", 0, [])
        , ("Unsorted list, free at front", 0, [2,3,1])
        , ("Unsorted list, free in middle", 2, [1,0,3])
        , ("Unsorted list, free at end", 3, [2,1,0])
        , ("Unsorted list with duplicates, free at front", 0, [2,2,3,1,3])
        , ("Unsorted list with duplicates, free in middle", 2, [0,1,0,3,1])
        , ("Unsorted list with duplicates, free in middle", 2, [0,1,1,0,4,7,12,14,342,1,1,1,3,4,1])
        , ("Unsorted list with duplicates, free at end", 3, [2,1,0,0,2])
        ]

testFns f = map (TestCase . assertEqual' f) tests
    where assertEqual' f (desc, expected, args) = assertEqual desc expected (f args)

main = runTestTT $ TestList [ TestList (testFns minfree_bv)
                            , TestList (testFns minfree_chl)
                            , TestList (testFns minfree_col)
                            , TestList (testFns minfree_b)
                            , TestList (testFns minfree_r)
                            , TestList (testFns minfree_o)
                            , TestList (testFns minfree_bhof)
                            , TestList (testFns minfree_rhof)
                            , TestList (testFns minfree_ohof)
                            ]
