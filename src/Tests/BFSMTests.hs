module Tests.BFSMTests where

import BFSM
import FSM 

import Tests.Tests
import Test.HUnit hiding (State) 

bfsTests :: Test
bfsTests = TestList [bfsTest1, bfsTest2, bfsTest3, bfsTest4]

dobfsTests :: IO Counts
dobfsTests = runTestTT bfsTests

bfsTest1 :: Test
bfsTest1 = "Check successful for finding paths"
        ~: Just [(0, Nothing), (1, Just A), (3, Just B)] ~=? doBFS testFSM

bfsTest2 :: Test
bfsTest2 = "Check that we can correctly extract call paths"
        ~: Just [A, B] ~=? extractCalls (doBFS testFSM)

bfsTest3 :: Test
bfsTest3 = "Check that for a not-accepting FSM we return nothing"
        ~: Nothing ~=? doBFS notAcceptFSM

bfsTest4 :: Test
bfsTest4 = "Check that we have no call path for above"
        ~: Nothing ~=? extractCalls (doBFS notAcceptFSM)

compTrans = intersectionFSM [inters1, inters2]

data FChar = A | B deriving (Eq, Show)

testFSM :: FSM FChar Integer 
testFSM = FSM 
    [A, B]
    [0, 1, 2, 3]
    testFSMTrans
    [0]
    testFSMAccepting

testFSMTrans :: Transition Integer FChar
testFSMTrans (0, A) = Just 1
testFSMTrans (0, B) = Just 2
testFSMTrans (1, A) = Just 1
testFSMTrans (1, B) = Just 3
testFSMTrans (2, A) = Just 2
testFSMTrans (2, B) = Just 2
testFSMTrans (3, A) = Just 3
testFSMTrans (3, B) = Just 3
testFSMTrans _      = Nothing

testFSMAccepting :: Integer -> Bool
testFSMAccepting 3 = True
testFSMAccepting _ = False

notAcceptFSM :: FSM FChar Integer
notAcceptFSM = FSM 
    [A, B]
    [0, 1, 2, 3]
    testFSMTrans
    [0]
    (const False)

inters1 :: FSM FChar Integer
inters1 = FSM
  [A, B]
  [0, 1, 2]
  inters1t
  [0]
  acc2

acc2 :: Integer -> Bool
acc2 2 = True
acc2 _ = False

inters1t :: Transition Integer FChar
inters1t (0, A) = Just 1
inters1t (0, B) = Just 0
inters1t (1, A) = Just 2
inters1t (1, B) = Just 0
inters1t (2, A) = Just 2
inters1t (2, B) = Just 2

inters2 :: FSM FChar Integer
inters2 = FSM
  [A, B]
  [0, 1, 2, 3]
  inters2t
  [0]
  acc3

acc3 :: Integer -> Bool
acc3 3 = True
acc3 _ = False

inters2t :: Transition Integer FChar
inters2t (0, A) = Just 1
inters2t (0, B) = Just 2
inters2t (1, A) = Just 2
inters2t (1, B) = Just 3
inters2t (2, A) = Just 3
inters2t (2, B) = Just 3
inters2t (3, A) = Just 3
inters2t (3, B) = Just 3

  
