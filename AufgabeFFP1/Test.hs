module FFPTest where

import AufgabeFFP1
import Test.HUnit

pof2sHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded list"
    [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192]
    (take 14 pof2s)

pof2sGenerated = TestCase $ assertEqual
    "Head should equal generated list"
    (take 512 (map (2^) [0..]))
    (take 512 pof2s)

pof2sGroup = TestList [ pof2sHardcoded,
                        pof2sGenerated ]

pdHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded list"
    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1],
     [1,7,21,35,35,21,7,1],[1,8,28,56,70,56,28,8,1]]
    (take 9 pd)

pdGroup = TestList [ pdHardcoded ]

fibdiagZero = TestCase $ assertEqual
    "Invalid arg returns empty list"
    []
    (fibdiag 0)

fibdiagNeg = TestCase $ assertEqual
    "Invalid arg returns empty list"
    []
    (fibdiag (-1))

diags = [[1],[1],[1,1],[1,2],[1,3,1],[1,4,3],[1,5,6,1],[1,6,10,4]]

fibdiagHardcoded = TestCase $ assertEqual
    "Result should equal hardcoded list"
    diags
    (map fibdiag [1..8])

fibdiagGroup = TestList [ fibdiagZero,
                          fibdiagNeg,
                          fibdiagHardcoded ]

fibdiagsHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded list"
    diags
    (take 8 fibdiags)

fibdiagsGroup = TestList [ fibdiagsHardcoded ]

fibspdHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded list"
    (map sum diags)
    (take 8 fibspd)

fibspdGroup = TestList [ fibspdHardcoded ]

main = runTestTT $ TestList [ pof2sGroup,
                              pdGroup,
                              fibdiagGroup,
                              fibdiagsGroup,
                              fibspdGroup ]
