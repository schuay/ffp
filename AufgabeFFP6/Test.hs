module Main where

import AufgabeFFP6
import Test.HUnit

test = TestCase $ assertEqual
    "Description"
    (expected)
    (actual)


group = TestList [ test ]

main = runTestTT $ TestList [ group ]
