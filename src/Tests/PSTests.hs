module Tests.PSTests where

import Model
import FSM
import BFSM
import FST 
import ME 
import RS
import Powerset

import Tests.Tests
import Test.HUnit hiding (State)

--psTests :: Test
--psTests = TestList [psTest1, psTest2, psTest3, psTest4, psTest5, 
--                    psTest6, psTest7, psTest8, psTest9, psTest10]

psTests :: Test
psTests = TestList [psTest1, psTest2, psTest3, psTest4, psTest5,
                    psTest6, psTest7, psTest8, psTest9, psTest10,
                    psTest11]

dopsTests :: IO Counts
dopsTests = runTestTT psTests

psTest1 :: Test
psTest1 = "Test that transition for non-related states is correct" 
       ~: Just (PCon (PVar (Q [N a b, N b a, S a b, S b a])) [PVar (Q [N a b, N b a, S a b, S b a])]) ~=? t1

psTest2 :: Test 
psTest2 = "Test that transition for related states is correct"
       ~: Just (PCon (PVar (Q [N a b, N b a, S a b, S b a])) [PVar (Q [N a b, N b a, S a b, S b a]), PVar (Q [N b a])]) ~=? t2

psTest3 :: Test
psTest3 = "There should be no duplicates for related states"
       ~: Just (PCon (PVar (Q [N a b, N b a, S a b, S b a])) [PVar (Q [N a b, N b a, S a b, S b a])]) ~=? t5

psTest4 :: Test
psTest4 = "Check that PEval is working right, positively"
       ~: True ~=? evalState (K a (allExpertsAg [a, b])) (PCon (PVar (Q [N a b, N b a, S a b, S b a])) [PVar (Q [N a b, N b a, S a b, S b a])])

psTest5 :: Test
psTest5 = "Check that PEval is working right, negatively"
       ~: False ~=? evalState  (K a (allExpertsAg [a, b])) (PCon (PVar (Q [N a b, N b a, S a b, S b a])) [PVar (Q [N a b, N b a, S a b, S b a]), PVar (Q [N a b])])

psTest6 :: Test
psTest6 = "Check that we don't have any duplicates in the indistinguishable worlds"
       ~: Just (PCon (PVar (Q [N b a])) [PVar (Q [N b a])]) ~=? powersetTrans (PCon (PVar (Q [N b a])) [PVar (Q [N b a]), PVar (Q [N b a])], Right (Call b b))

psTest7 :: Test
psTest7 = "Check that we can correctly identify winning paths"
        ~: True ~=? existsWinningPath powerset [PCon (PVar (Q [N a b])) [PVar (Q [N a b])]]

psTest8 :: Test
psTest8 = "Make sure calls update states properly"
       ~: Just (PCon (PVar (Q [N a b, N b a, S a b, S b a])) [PVar (Q [N a b, N b a, S a b, S b a]), PVar (Q [N b a])]) ~=? powersetTrans (PCon (PVar (Q [N b a])) [PVar (Q [N a b, N b a, S a b, S b a]), PVar (Q [N b a])], Right (Call b a))

psTest9 :: Test
psTest9 = "Make sure calls update states properly"
       ~: Just (PCon (PVar (Q [N b a])) [PVar (Q [N a b, N b a, S a b, S b a]), PVar (Q [N b a])]) ~=? powersetTrans (PCon (PVar (Q [N b a])) [PVar (Q [N a b, N b a, S a b, S b a]), PVar (Q [N b a])], Right (Call a a))

psTest10 :: Test
psTest10 = "Check that call string finding works fine"
        ~: Just [Right (Call b a), Right (Call a b)] ~=? extractCalls (doBFS psetBA)

psTest11 :: Test
psTest11 = "Check that we find a good result for PS3"
        ~: Just [Right (Call b c), Right (Call a b), Right (Call c b)] ~=? extractCalls (doBFS psetThree)

t1 = powersetTrans (PCon (PVar $ Q [N a b]) [PVar $ Q [N a b]], Right (Call a b))
t2 = powersetTrans (PCon (PVar $ Q [N b a]) [PVar $ Q [N b a]], Right (Call b a))
--t2' = powersetTrans (PCon (Q [N b a]) [Q [N b a]], Right (Call a a))

-- It's becoming time to consider what to do for a non-permitted call
-- It seems to make the most sense to just return the empty list, thus making a 
-- "non-transition" in the automata. 
-- However, we need to consider what the implications of this will be elsewhere. 
-- Will this be problematic for other cases
--t3 = powersetTrans (PCon (Q [N b a]) [Q [N b a], Q [N b a]], Right (Call b a))
--t4 = powersetTrans (PCon (Q [N b a]) [Q [N b a], Q [N b a], Q [N b a, S b c]], Right (Call b a))
t5 = powersetTrans (PCon (PVar $ Q [N b a, S b a, N a b, S a b]) [PVar $ Q [N b a, S b a, N a b, S a b]], Right (Call b a))

------------------------------------------------------------------------------

powersetTrans :: Transition (PState QState) Character
powersetTrans = transition powerset

powerset :: FSM Character (PState QState)
powerset = setSuccessfulFormula (K a (allExpertsAg [a, b])) $ psaFromScratch a model eventModel

psetBA :: FSM Character (PState QState)
psetBA = setInitial [PCon (PVar $ Q [N b a]) [PVar $ Q [N b a]]] $ setStatesReachable [PCon (PVar $ Q [N b a]) [PVar $ Q [N b a]]] powerset

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

-----------------------------------------------------------

threeModel :: EpistM
threeModel = Mo 
    [State (0, [])]
    [a, b, c]
    [(State (0, []), [P (N a b), P (N b c)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]])]
    [State (0, [])]

threeEvModel :: EventModel
threeEvModel = standardEventModel [a, b, c] anyCall postUpdate

psetThree :: FSM Character (PState QState)
psetThree = setStatesReachableInit $ 
            setInitial [PCon (PVar $ Q [N a b, N b c]) [PVar $ Q [N a b, N b c]]] $
            setSuccessfulFormula (K a (allExpertsAg [a, b, c])) $ 
            psaFromScratch a threeModel threeEvModel

--ppset :: FSM Character (PState (PState QState))
--ppset = buildPSA psetThree (liftTransducer (buildComposedSS b threeModel threeEvModel (buildDAutomata threeModel threeEvModel)))

fourModel :: EpistM 
fourModel = Mo
    [State (0, [])]
    [a, b, c, d]
    [(State (0, []), [P (N a b), P (N b c), P (N c d)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]])]
    [State (0, [])]

fourEvModel :: EventModel
fourEvModel = standardEventModel [a, b, c, d] anyCall postUpdate

-- For everyone expert, Just [Right Ag c Ag d,Right Ag b Ag c,Right Ag a Ag b,Right Ag c Ag b,Right Ag d Ag c]
-- For K_a Expert,      Just [Right Ag b Ag c,Right Ag a Ag b,Right Ag b Ag d,Right Ag d Ag a,Right Ag a Ag c]
psetFour :: FSM Character (PState QState)
psetFour = setStatesReachableInit $
           setInitial [PCon (PVar $ Q [N a b, N b c, N c d]) [PVar $ Q [N a b, N b c, N c d]]] $
           setSuccessfulFormula (K a (allExpertsAg [a, b, c, d])) $
           psaFromScratch a fourModel fourEvModel

-----------------------------------------------------------

--dAuto3 :: FSM Character QState
--dAuto3 = setStatesReachableInit . setInitial [Q [N a b, N b c]] $ buildDAutomata threeModel threeEvModel

--rs3 :: RegularStructure Character QState
--rs3 = RegularStructure dAuto3 undefined undefined






