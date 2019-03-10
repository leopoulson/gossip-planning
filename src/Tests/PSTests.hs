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
psTests = TestList [psTest1, psTest2, psTest3, psTest4, psTest5]

dopsTests :: IO Counts
dopsTests = runTestTT psTests

psTest1 :: Test
psTest1 = "Test that transition for non-related states is correct" 
       ~: PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a]] ~=? t1

psTest2 :: Test 
psTest2 = "Test that transition for related states is correct"
       ~: PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a], Q [N b a]] ~=? t2

psTest3 :: Test
psTest3 = "There should be no duplicates for related states"
       ~: PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a]] ~=? t5

psTest4 :: Test
psTest4 = "Check that PEval is working right, positively"
       ~: True ~=? evalPState  (K a (allExpertsAg [a, b])) (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a]])

psTest5 :: Test
psTest5 = "Check that PEval is working right, negatively"
       ~: False ~=? evalPState  (K a (allExpertsAg [a, b])) (PState (Q [N a b, N b a, S a b, S b a]) [Q [N a b, N b a, S a b, S b a], Q [N a b]])


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


-- psTest3 :: Test 
-- psTest3 = "Test that result is identical for indistinguishable calls"

powersetTrans :: Transition PState Character
powersetTrans = transition powerset

powerset :: FSM Character PState
powerset = psaFromScratch a model eventModel

model :: EpistM
model = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a, b, c]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]]), (b, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]

eventModel :: EventModel
eventModel = EvMo
    [Call a b, Call b a, Call a a]
    [(a, [[Call a b], [Call b a, Call a a]]), (b, [[Call a b], [Call b a, Call a a]])]
    anyCall
    postUpdate

