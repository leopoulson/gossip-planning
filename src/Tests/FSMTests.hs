module Tests.FSMTests where

import Test.HUnit hiding (State)

import Model
import ME
import FSM
import Tests.Tests

data FChar = A | B deriving (Eq, Show)

testFSM :: FSM FChar Integer
testFSM = FSM 
    [A, B]
    [0, 1, 2, 3]
    trans1
    [0]
    accept

trans1 :: Transition Integer FChar
trans1 (0, A) = Just 1
trans1 (0, B) = Just 2
trans1 (1, A) = Just 3
trans1 (1, B) = Just 3
trans1 _      = Nothing

accept :: Integer -> Bool
accept 3 = True
accept _ = False

-- Basic FSM testing 

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8]

doTests :: IO Counts
doTests = runTestTT tests

test1 :: Test
test1 = "Check that allTrans works positively"
     ~: [1, 2] ~=? findReachableFromOne testFSM 0

test2 :: Test
test2 = "Check that allTrans works negatively"
     ~: [] ~=? findReachableFromOne testFSM 2

test3 :: Test 
test3 = "Testing findReachableStates" 
     ~: [0, 1, 2, 3] ~=? findReachableFromSet testFSM [0, 1]

test4 :: Test 
test4 = "Testing findReachableStates from 2"
     ~: [2] ~=? findReachableFromSet testFSM [2]

test5 :: Test
test5 = "Positively test findPathReachable"
     ~: [0, 1, 2, 3] ~=? findPathReachable testFSM [0]

test6 :: Test
test6 = "Negatively test findPathReachable"
     ~: [2] ~=? findPathReachable testFSM [2]

test7 :: Test
test7 = "Positively test winning path"
     ~: True ~=? existsWinningPath testFSM [0]

test8 :: Test
test8 = "Negatively test winning path"
     ~: False ~=? existsWinningPath testFSM [2]

----------- Testing Loops -----------------

loopFSM :: FSM FChar Integer
loopFSM = FSM 
    [A, B]
    [0, 1, 2, 3]
    loopTrans
    [0]
    loopAccept

loopTrans :: Transition Integer FChar
loopTrans (0, A) = Just 0
loopTrans (0, B) = Just 1
loopTrans _ = Nothing

loopAccept :: Integer -> Bool
loopAccept 0 = True
loopAccept _ = False

lTests :: Test
lTests = TestList [lTest1, lTest2, lTest3]

dolTests :: IO Counts
dolTests = runTestTT lTests

lTest1 :: Test
lTest1 = "Check find loops works"
      ~: [(0, A)] ~=? findLoops loopFSM

lTest2 :: Test 
lTest2 = "Check remove loops works"
      ~: Nothing ~=? transition (removeLoopsFSM loopFSM) (0, A)

lTest3 :: Test
lTest3 = "Check no loops left after remove loops"
      ~: [] ~=? (findLoops . removeLoopsFSM) loopFSM 





