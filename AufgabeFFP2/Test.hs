module Main where

import AufgabeFFP2
import Test.HUnit

ppsHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded values"
    [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
    (take 10 pps)

pps20th = TestCase $ assertEqual
    "Single element should equal hardcoded value"
    (347,349)
    (pps !! 20)

pps31st = TestCase $ assertEqual
    "Single element should equal hardcoded value"
    (809, 811)
    (pps !! 30)

ppsGroup = TestList [ ppsHardcoded, pps20th, pps31st ]

powFastNeg = TestCase $ assertEqual
    "Negative n should return 1"
    1
    (powFast (-1))

powFastZero = TestCase $ assertEqual
    "Zero n should return 1"
    1
    (powFast 0)

pow' 0 = 1
pow' n = pow' (n - 1) + pow' (n - 1)

powFastGenerated = TestCase $ assertEqual
    "Head should equal generated values"
    (map pow' [0..15])
    (map powFast [0..15])

powFastGroup = TestList [ powFastNeg, powFastZero, powFastGenerated ]

fKNeg = TestCase $ assertEqual
    "Negative k should return 0"
    0
    (f 2 (-1))

fKZero = TestCase $ assertEqual
    "k == 0 should return 1"
    1
    (f 2 0)

fRange = [(-30)..30] :: [Int]
fKOne = TestCase $ assertEqual
    "k == 1 should return z + 1"
    (map (fromIntegral . (+1)) fRange)
    (map (\x -> f x 1) fRange)

fMTKNeg = TestCase $ assertEqual
    "Negative k should return 0"
    0
    (fMT 2 (-1))

fMTKZero = TestCase $ assertEqual
    "k == 0 should return 1"
    1
    (fMT 2 0)

fMTKOne = TestCase $ assertEqual
    "k == 1 should return z + 1"
    (map (fromIntegral . (+1)) fRange)
    (map (\x -> fMT x 1) fRange)

fEqualsfMT = TestCase $ assertEqual
    "f should always equal fMT"
    [ f z k | z <- fRange, k <- [0..20] ]
    [ fMT z k | z <- fRange, k <- [0..20] ]

fHardcoded1 = TestCase $ assertEqual
    "f should equal hardcoded values"
    [1.0,1.0,1.0,1.0,2.0]
    (take 5 [f i j|k<-[0..],i<-[0..k],j<-[k-i]])

fHardcoded2 = TestCase $ assertEqual
    "f should equal hardcoded values"
    "[1.0,1.0,2.716667,7.0,13.0]"
    ((show . take 5 . drop 20) [f i j|k<-[0..],i<-[0..k],j<-[k-i]])

fHardcoded3 = TestCase $ assertEqual
    "f should equal hardcoded values"
    "[7.266667,16.375,23.66667,18.5,7.0]"
    ((show . take 5 . drop 30) [f i j|k<-[0..],i<-[0..k],j<-[k-i]])

fHardcoded4 = TestCase $ assertEqual
    "f should equal hardcoded values"
    [65375,61000,32500,9000,1000]
    ((take 5 . drop 50)[truncate (1000*f i j)|k<-[0..],i<-[0..k],j<-[k-i]])

fMTHardcoded1 = TestCase $ assertEqual
    "f should equal hardcoded values"
    [1.0,1.0,1.0,1.0,2.0]
    (take 5 [fMT i j|k<-[0..],i<-[0..k],j<-[k-i]])

fMTHardcoded2 = TestCase $ assertEqual
    "f should equal hardcoded values"
    "[1.0,1.0,2.716667,7.0,13.0]"
    ((show . take 5 . drop 20) [fMT i j|k<-[0..],i<-[0..k],j<-[k-i]])

fMTHardcoded3 = TestCase $ assertEqual
    "f should equal hardcoded values"
    "[7.266667,16.375,23.66667,18.5,7.0]"
    ((show . take 5 . drop 30) [fMT i j|k<-[0..],i<-[0..k],j<-[k-i]])

fMTHardcoded4 = TestCase $ assertEqual
    "f should equal hardcoded values"
    [65375,61000,32500,9000,1000]
    ((take 5 . drop 50)[truncate (1000*fMT i j)|k<-[0..],i<-[0..k],j<-[k-i]])

fGroup = TestList [ fKNeg, fKZero, fKOne, fMTKNeg, fMTKZero, fMTKOne, fEqualsfMT,
                    fHardcoded1, fHardcoded2, fHardcoded3, fHardcoded4,
                    fMTHardcoded1, fMTHardcoded2, fMTHardcoded3, fMTHardcoded4 ]

gzNeg = TestCase $ assertEqual
    "Negative n should equal 0"
    0
    (gz (-1))

gzZero = TestCase $ assertEqual
    "n == 0 should equal 0"
    0
    (gz 0)

gzHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded values"
    [2,4,8,2,6,144,400]
    (map gz [1,2,3,10,11,42,402])

gzsHardcoded = TestCase $ assertEqual
    "Head should equal hardcoded values"
    [2,4,8,16,32,64,128,256,512,2,6]
    (take 11 gzs)

gzGroup = TestList [ gzNeg, gzZero, gzHardcoded, gzsHardcoded ]

main = runTestTT $ TestList [ ppsGroup, powFastGroup, fGroup, gzGroup ]
