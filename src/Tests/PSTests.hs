module Tests.PSTests where

import Model
import FSM
import FST 
import SSFST
import ME 
import Powerset

import Tests.Tests
import Test.HUnit hiding (State)

psTests :: Test
psTests = TestList [psTest1, psTest2, psTest3, psTest4, psTest5, psTest6, psTest7, psTest8]

dopsTests :: IO Counts
dopsTests = runTestTT psTests

psTest1 :: Test
psTest1 = "Test that transition for non-related states is correct" 
       ~: Just (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a]]) ~=? t1

psTest2 :: Test 
psTest2 = "Test that transition for related states is correct"
       ~: Just (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a], Q [N b a]]) ~=? t2

psTest3 :: Test
psTest3 = "There should be no duplicates for related states"
       ~: Just (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a]]) ~=? t5

psTest4 :: Test
psTest4 = "Check that PEval is working right, positively"
       ~: True ~=? evalPState  (K a (allExpertsAg [a, b])) (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a]])

psTest5 :: Test
psTest5 = "Check that PEval is working right, negatively"
       ~: False ~=? evalPState  (K a (allExpertsAg [a, b])) (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a], Q [N a b]])

psTest6 :: Test
psTest6 = "Check that we don't have any duplicates in the indistinguishable worlds"
       ~: Just (PState (Q [N b a]) [Q [N b a]]) ~=? powersetTrans (PState (Q [N b a]) [Q [N b a], Q [N b a]], Right (Call b b))

psTest7 :: Test
psTest7 = "Check that we can correctly identify winning paths"
       ~: True ~=? existsWinningPath powerset [PState (Q [N a b]) [Q [N a b]]]

psTest8 :: Test
psTest8 = "Check that we can negatively find winning paths"
       ~: False ~=? existsWinningPath powerset [PState (Q [N b a]) [Q [N b a]]]

t1 = powersetTrans (PState (Q [N a b]) [Q [N a b]], Right (Call a b))
t2 = powersetTrans (PState (Q [N b a]) [Q [N b a]], Right (Call b a))
t2' = powersetTrans (PState (Q [N b a]) [Q [N b a]], Right (Call a a))

-- It's becoming time to consider what to do for a non-permitted call
-- It seems to make the most sense to just return the empty list, thus making a 
-- "non-transition" in the automata. 
-- However, we need to consider what the implications of this will be elsewhere. 
-- Will this be problematic for other cases
t3 = powersetTrans (PState (Q [N b a]) [Q [N b a], Q [N b a]], Right (Call b a))
t4 = powersetTrans (PState (Q [N b a]) [Q [N b a], Q [N b a], Q [N b a, S b c]], Right (Call b a))
t5 = powersetTrans (PState (Q [N b a, S b a, N a b, S a b]) [Q [N b a, S b a, N a b, S a b]], Right (Call b a))

------------------------------------------------------------------------------

t6 = findReachableFromSet powerset [PState (Q [N b a]) [Q [N b a]]]
t7 = findReachableFromSet powerset [PState (Q [N a b]) [Q [N a b]]]

t8 = FSM.accepting powerset $ PState (Q [N b a]) [Q [N b a]] -- PState (Q [N a b,N b a,S a b,S b a]) [Q [N a b,N b a,S a b,S b a],Q [N b a]]

-- psTest3 :: Test 
-- psTest3 = "Test that result is identical for indistinguishable calls"

powersetTrans :: Transition PState Character
powersetTrans = transition powerset

powerset :: FSM Character PState
powerset = setSuccessfulFormula (K a (allExpertsAg [a, b])) $ psaFromScratch a model eventModel

model :: EpistM
model = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a, b, c]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]]), (b, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]

eventModel :: EventModel
eventModel = EvMo
    [Call a b, Call b a, Call a a, Call b b]
    [(a, [[Call a b], [Call b a, Call a a], [Call b b]]), (b, [[Call a b], [Call b a, Call a a], [Call b b]])]
    anyCall
    postUpdate

