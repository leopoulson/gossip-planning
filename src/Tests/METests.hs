module Tests.METests where

import ME 
import Model
import FSM
import Tests.Tests

import Test.HUnit hiding (State)

-- Tests for ME* construction

tStates :: Test
tStates = "Check that the states are enumerated correctly"
      ~: getQStates exampleModel ~=? FSM.states dAutomata

tTrans1 :: Test
tTrans1 = "Check that transition is working as it should, from initial state"
       ~: transition dAutomata (QInit, Left (State (0, []))) ~=? Q [S a b, N a b]

tTrans2 :: Test
tTrans2 = "Test transition updated by call"
       ~: Q [S a b, S b a, N a b, N b a] ~=? transition dAutomata (Q [S a b, N a b], Right (Call a b))

tTrans3 :: Test
tTrans3 = "Test transition updated by call when everyone knows everything"
       ~: Q [S a b, S b a, N a b, N b a] ~=? transition dAutomata (Q [S a b, S b a, N a b, N b a], Right (Call a b))


meTests :: Test
meTests = test [tStates, tTrans1, tTrans2, tTrans3]

domeTest :: IO Counts
domeTest = runTestTT meTests

exampleModel :: EpistM
exampleModel = Mo 
    [State (0, [])]
    [a, b]
    [(State (0, []), [P (S a b), P (N a b)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]])]
    [State (0, [])]

eventModel :: EventModel
eventModel = EvMo 
    [Call a b, Call b a] 
    [(a, [[Call a b], [Call b a]]), (b, [[Call a b], [Call b a]])] 
    anyCall 
    postUpdate

relUpdate :: EpistM
relUpdate = update exampleModel eventModel

dAutomata :: FSM Character QState
dAutomata = buildDAutomata exampleModel eventModel















