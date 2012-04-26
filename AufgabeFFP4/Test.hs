module Main where

import AufgabeFFP4
import Test.HUnit

test = TestCase $ assertEqual
    "Description"
    (expected)
    (actual)


group = TestList [ test ]

main = runTestTT $ TestList [ group ]
