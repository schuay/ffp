-- TODO: find a way to add the QuickCheck tests
module Main where

import AufgabeFFP7
import Test.HUnit

testEmpty = TestCase $ assertEqual
    "Empty buffer must be empty"
    (0, [])
    (empty)

testInsert1 = TestCase $ assertEqual
    "Insert into empty buffer"
    (1, "a")
    (insert 'a' (empty))

testInsert2 = TestCase $ assertEqual
    "Insert before start"
    (1, "abc")
    (insert 'a' (-1, "bc"))

testInsert3 = TestCase $ assertEqual
    "Insert at start"
    (1, "abc")
    (insert 'a' (0, "bc"))

testInsert4 = TestCase $ assertEqual
    "Insert somewhere"
    (2, "abc")
    (insert 'b' (1, "ac"))

testInsert5 = TestCase $ assertEqual
    "Insert at end"
    (3, "abc")
    (insert 'c' (2, "ab"))

testInsert6 = TestCase $ assertEqual
    "Insert after end"
    (3, "abc")
    (insert 'c' (5, "ab"))

testEmptyI = TestCase $ assertEqual
    "Empty buffer must be empty"
    ([], [])
    (emptyI)

testInsertI1 = TestCase $ assertEqual
    "Insert into empty buffer"
    ("a", [])
    (insertI 'a' (emptyI))

testInsertI2 = TestCase $ assertEqual
    "Insert at start"
    ("a", "bc")
    (insertI 'a' ([], "bc"))

testInsertI3 = TestCase $ assertEqual
    "Insert somewhere"
    ("ba", "c")
    (insertI 'b' ("a", "c"))

testInsertI4 = TestCase $ assertEqual
    "Insert at end"
    ("cba", [])
    (insertI 'c' ("ba", []))

testDelete1 = TestCase $ assertEqual
    "Delete from empty buffer"
    (0, [])
    (delete (empty))

testDelete2 = TestCase $ assertEqual
    "Delete before start"
    (0, "abc")
    (delete (-1, "abc"))

testDelete3 = TestCase $ assertEqual
    "Delete at start"
    (0, "abc")
    (delete (0, "abc"))

testDelete4 = TestCase $ assertEqual
    "Delete somewhere"
    (1, "ac")
    (delete (2, "abc"))

testDelete5 = TestCase $ assertEqual
    "Delete at end"
    (2, "ab")
    (delete (3, "abc"))

testDelete6 = TestCase $ assertEqual
    "Delete after end"
    (3, "abc")
    (delete (5, "abc"))

testDeleteI1 = TestCase $ assertEqual
    "DeleteI from empty buffer"
    ([], [])
    (deleteI (emptyI))

testDeleteI2 = TestCase $ assertEqual
    "DeleteI at start"
    ([], "abc")
    (deleteI ([], "abc"))

testDeleteI3 = TestCase $ assertEqual
    "DeleteI somewhere"
    ("a", "c")
    (deleteI ("ba", "c"))

testDeleteI4 = TestCase $ assertEqual
    "DeleteI at end"
    ("ba", [])
    (deleteI ("cba", []))

testLeft1 = TestCase $ assertEqual
    "Move left at start"
    (0, "abc")
    (left (0, "abc"))

testLeft2 = TestCase $ assertEqual
    "Move left"
    (1, "abc")
    (left (2, "abc"))

testLeftI1 = TestCase $ assertEqual
    "Move left at start"
    ([], "abc")
    (leftI ([], "abc"))

testLeftI2 = TestCase $ assertEqual
    "Move left"
    ("a", "bc")
    (leftI ("ba", "c"))

testRight1 = TestCase $ assertEqual
    "Move right at end"
    (3, "abc")
    (right (3, "abc"))

testRight2 = TestCase $ assertEqual
    "Move right"
    (2, "abc")
    (right (1, "abc"))

testRightI1 = TestCase $ assertEqual
    "Move right at end"
    ("abc", [])
    (rightI ("abc", []))

testRightI2 = TestCase $ assertEqual
    "Move right"
    ("ba", "c")
    (rightI ("a", "bc"))

testAtLeft1 = TestCase $ assertEqual
    "Not left"
    (False)
    (atLeft (1, "abc"))

testAtLeft2 = TestCase $ assertEqual
    "Left"
    (True)
    (atLeft (0, "abc"))

testAtRight1 = TestCase $ assertEqual
    "Not right"
    (False)
    (atRight (1, "abc"))

testAtRight2 = TestCase $ assertEqual
    "Right"
    (True)
    (atRight (3, "abc"))

testAtLeftI1 = TestCase $ assertEqual
    "Not left"
    (False)
    (atLeftI ("a", "bc"))

testAtLeftI2 = TestCase $ assertEqual
    "Left"
    (True)
    (atLeftI ([], "abc"))

testAtRightI1 = TestCase $ assertEqual
    "Not right"
    (False)
    (atRightI ("a", "bc"))

testAtRightI2 = TestCase $ assertEqual
    "Right"
    (True)
    (atRightI ("abc", []))

testRetrieve1 = TestCase $ assertEqual
    "Retrieve empty buffer"
    (0, [])
    (retrieve (emptyI))

testRetrieve2 = TestCase $ assertEqual
    "Retrieve buffer, cursor at start"
    (0, "abc")
    (retrieve ([], "abc"))

testRetrieve3 = TestCase $ assertEqual
    "Retrieve buffer, cursor at end"
    (3, "abc")
    (retrieve ("cba", []))

testRetrieve4 = TestCase $ assertEqual
    "Retrieve buffer"
    (2, "abc")
    (retrieve ("ba", "c"))

testSsfn1 = TestCase $ assertEqual
    "Empty list"
    (0)
    (ssfn [])

testSsfn2 = TestCase $ assertEqual
    "Unsorted list, free at front"
    (0)
    (ssfn [2,3,1])

testSsfn3 = TestCase $ assertEqual
    "Unsorted list, free in middle"
    (2)
    (ssfn [1,0,3])

{- TODO: reference code doesn't solve this correctly, even though it should.
 - (Return the smallest natural number not in the list -}
testSsfn4 = TestCase $ assertEqual
    "Unsorted list, free at end"
    (3)
    (ssfn [2,1,0])

testSsfn5 = TestCase $ assertEqual
    "Unsorted list with duplicates, free at front"
    (0)
    (ssfn [2,2,3,1,3])

testSsfn6 = TestCase $ assertEqual
    "Unsorted list with duplicates, free in middle"
    (2)
    (ssfn [0,1,0,3,1])

{- TODO: reference code doesn't solve this correctly, even though it should.
 - (Return the smallest natural number not in the list -}
testSsfn7 = TestCase $ assertEqual
    "Unsorted list with duplicates, free at end"
    (3)
    (ssfn [2,1,0,0,2])

testMinfree1 = TestCase $ assertEqual
    "Empty list"
    (0)
    (minfree [])

testMinfree2 = TestCase $ assertEqual
    "Unsorted list, free at front"
    (0)
    (minfree [2,3,1])

testMinfree3 = TestCase $ assertEqual
    "Unsorted list, free in middle"
    (2)
    (minfree [1,0,3])

testMinfree4 = TestCase $ assertEqual
    "Unsorted list, free at end"
    (3)
    (minfree [2,1,0])

testMinfree5 = TestCase $ assertEqual
    "Unsorted list with duplicates, free at front"
    (0)
    (minfree [2,2,3,1,3])

testMinfree6 = TestCase $ assertEqual
    "Unsorted list with duplicates, free in middle"
    (2)
    (minfree [0,1,0,3,1])

testMinfree7 = TestCase $ assertEqual
    "Unsorted list with duplicates, free at end"
    (3)
    (minfree [2,1,0,0,2])

groupBuffer = TestList [ testEmpty, testInsert1, testInsert2, testInsert3, testInsert4
                       , testInsert5, testInsert6, testDelete1, testDelete2, testDelete3
		       , testDelete4, testDelete5, testDelete6, testLeft1, testLeft2
		       , testRight1, testRight2, testAtLeft1, testAtLeft2, testAtRight1
		       , testAtRight2 ]
groupBufferI = TestList [ testEmptyI, testInsertI1, testInsertI2, testInsertI3
                        , testInsertI4, testDeleteI1, testDeleteI2, testDeleteI3
			, testDeleteI4, testLeftI1, testLeftI2, testRightI1, testRightI2
			, testAtLeftI1, testAtLeftI2, testAtRightI1, testAtRightI2
			, testRetrieve1, testRetrieve2, testRetrieve3, testRetrieve4 ]

groupSsfn = TestList [ testSsfn1, testSsfn2, testSsfn3, testSsfn4, testSsfn5, testSsfn6
                     , testSsfn7 ]
groupMinfree = TestList [ testMinfree1, testMinfree2, testMinfree3, testMinfree4
                        , testMinfree5, testMinfree6, testMinfree7 ]

main = runTestTT $ TestList [ groupBuffer, groupBufferI, groupSsfn, groupMinfree ]
