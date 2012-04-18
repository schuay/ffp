module Main where

import AufgabeFFP3
import Test.HUnit

ppsHardcoded = TestCase $ assertEqual
    "" -- Description
    [ 0 ] -- Expected
    [ 0 ] -- Actual

ppsGroup = TestList []

main = runTestTT $ TestList [ ppsGroup ]
