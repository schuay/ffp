module Main where

import AufgabeFFP8
import Test.HUnit

uniqTests =
        [ ("Empty list", 0, [])
        , ("Unsorted list, free at front", 0, [2,3,1])
        , ("Unsorted list, free in middle", 2, [1,0,3])
        , ("Unsorted list, free in middle", 5, [1,0,3,10,2,11,20,103,4,6,12])
        , ("Unsorted list, free at end", 3, [2,1,0])
        ]
dupTests =
        [ ("Unsorted list with duplicates, free at front", 0, [2,2,3,1,3])
        , ("Unsorted list with duplicates, free in middle", 2, [0,1,0,3,1])
        , ("Unsorted list with duplicates, free in middle", 2, [0,1,1,0,4,7,12,14,342,1,1,1,3,4,1])
        , ("Unsorted list with duplicates, free at end", 3, [2,1,0,0,2])
        ]

testFns tests f = map (TestCase . assertEqual' f) tests
    where assertEqual' f (desc, expected, args) = assertEqual desc expected (f args)

main = runTestTT $ TestList [ TestList (testFns uniqTests minfree_bv)
                            , TestList (testFns uniqTests minfree_chl)
                            , TestList (testFns uniqTests minfree_col)
                            , TestList (testFns dupTests minfree_bv)
                            , TestList (testFns dupTests minfree_chl)
                            , TestList (testFns dupTests minfree_col)
                            , TestList (testFns uniqTests minfree_b)
                            , TestList (testFns uniqTests minfree_r)
                            , TestList (testFns uniqTests minfree_o)
                            , TestList (testFns uniqTests minfree_bhof)
                            , TestList (testFns uniqTests minfree_rhof)
                            , TestList (testFns uniqTests minfree_ohof)
                            ]
