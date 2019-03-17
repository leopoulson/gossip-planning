module Tests.FSTTests where

import FST
import FSM
import Model 
import ME 

import Tests.Tests 
import Test.HUnit hiding (State) 

tests :: Test
tests = TestList 
      [ "Check that for just one call, we get the correct results"
     ~: [[Right (Call a b)]] ~=? getWordTrans trans (Q [N a b]) [Right (Call a b)] 
      , "Check that for two indist. callls, we get correct"
     ~: [[Right (Call b a)], [Right (Call a a)]] ~=? getWordTrans trans (Q [N b a]) [Right (Call b a)]
     ,  "Check that for a string of two indist"
     ~: [[Right (Call b a), Right (Call b a)], [Right (Call b a), Right (Call a a)], [Right (Call a a), 
          Right (Call b a)], [Right (Call a a), Right (Call a a)]] ~=? getWordTrans trans (Q [N b a]) [Right (Call b a), Right (Call b a)]]





doTests :: IO Counts
doTests = runTestTT tests

-- powersetTrans :: Transition (PState QState) Character
-- powersetTrans = transition powerset

-- powerset :: FSM Character (PState QState)
-- powerset = setSuccessfulFormula (K a (allExpertsAg [a, b])) $ psaFromScratch a model eventModel

-- psetBA :: FSM Character (PState QState)
-- psetBA = setInitial [PState (Q [N b a]) [Q [N b a]]] $ setStatesReachable [PState (Q [N b a]) [Q [N b a]]] powerset

t1 = startWordTrans trans (Q [N b a]) [Right $ Call b a, Right $ Call a a]

dAuto :: FSM Character QState
dAuto = buildDAutomata model eventModel

trans :: FST Character QState
trans = buildComposedSS a model eventModel dAuto

model :: EpistM
model = Mo
    [State (0, [])]
    [a, b, c]
    [(State (0, []), [P (N a b)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]])]
    [State (0, [])]

eventModel :: EventModel
eventModel = EvMo
    [Call a b, Call b a, Call a a, Call b b]
    [(a, [[Call a b], [Call b a, Call a a], [Call b b]]), (b, [[Call a b], [Call b a, Call a a], [Call b b]])]
    anyCall
    postUpdate
