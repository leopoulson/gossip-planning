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
