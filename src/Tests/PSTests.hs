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
import Data.Set (fromList, toList, Set)
import Data.Maybe (fromMaybe, fromJust)

--psTests :: Test
--psTests = TestList [psTest1, psTest2, psTest3, psTest4, psTest5, 
--                    psTest6, psTest7, psTest8, psTest9, psTest10]

psTests :: Test
psTests = TestList [psTest1, psTest2, psTest3, psTest4, psTest5,
                    psTest6, psTest7, psTest8, psTest9, psTest10,
                    psTest11]

ppPState :: Maybe (PState QState) -> IO ()
ppPState (Just p) = putStr (prettyPrintPState p 0)
ppPState Nothing  = putStr "Nothing"

prettyPrintPState :: PState QState -> Int -> String
prettyPrintPState (PVar (Q set)) n = concat (replicate n "    ") ++ "Var: " ++ (show $ toList set) ++ "\n"
prettyPrintPState (PCon p ps)    n = concat (replicate n "    ") ++ "Con: \n" ++ prettyPrintPState p (n + 1) ++
                                     concat (replicate n "    ") ++ "Accessibles: \n" ++ concatMap (\p -> prettyPrintPState p (n+1)) ps

dopsTests :: IO Counts
dopsTests = runTestTT psTests

allTests :: Test
allTests = concatTests [psTests, bSATests]

runAllTests = runTestTT allTests

makeQ :: [Prop] -> QState
makeQ ps = Q (fromList ps)

psTest1 :: Test
psTest1 = "Test that transition for non-related states is correct" 
       ~: Just (PCon (PVar (makeQ [N a b, N b a, S a b, S b a])) [PVar (makeQ [N a b, N b a, S a b, S b a])]) ~=? t1

psTest2 :: Test 
psTest2 = "Test that transition for related states is correct"
       ~: Just (PCon (PVar (makeQ [N a b, N b a, S a b, S b a])) [PVar (makeQ [N a b, N b a, S a b, S b a]), PVar (makeQ [N b a])]) ~=? t2

psTest3 :: Test
psTest3 = "There should be no duplicates for related states"
       ~: Just (PCon (PVar (makeQ [N a b, N b a, S a b, S b a])) [PVar (makeQ [N a b, N b a, S a b, S b a])]) ~=? t5

psTest4 :: Test
psTest4 = "Check that PEval is working right, positively"
       ~: True ~=? evalState (K a (allExpertsAg [a, b])) (PCon (PVar (makeQ [N a b, N b a, S a b, S b a])) [PVar (makeQ [N a b, N b a, S a b, S b a])])

psTest5 :: Test
psTest5 = "Check that PEval is working right, negatively"
       ~: False ~=? evalState  (K a (allExpertsAg [a, b])) (PCon (PVar (makeQ [N a b, N b a, S a b, S b a])) [PVar (makeQ [N a b, N b a, S a b, S b a]), PVar (makeQ [N a b])])

psTest6 :: Test
psTest6 = "Check that we don't have any duplicates in the indistinguishable worlds"
       ~: Just (PCon (PVar (makeQ [N b a])) [PVar (makeQ [N b a])]) ~=? powersetTrans (PCon (PVar (makeQ [N b a])) [PVar (makeQ [N b a]), PVar (makeQ [N b a])], Right (Call b b))

psTest7 :: Test
psTest7 = "Check that we can correctly identify winning paths"
        ~: True ~=? existsWinningPath powerset [PCon (PVar (makeQ [N a b])) [PVar (makeQ [N a b])]]

psTest8 :: Test
psTest8 = "Make sure calls update states properly"
       ~: Just (PCon (PVar (makeQ [N a b, N b a, S a b, S b a])) [PVar (makeQ [N a b, N b a, S a b, S b a]), PVar (makeQ [N b a])]) ~=? powersetTrans (PCon (PVar (makeQ [N b a])) [PVar (makeQ [N a b, N b a, S a b, S b a]), PVar (makeQ [N b a])], Right (Call b a))

psTest9 :: Test
psTest9 = "Make sure calls update states properly"
       ~: Just (PCon (PVar (makeQ [N b a])) [PVar (makeQ [N a b, N b a, S a b, S b a]), PVar (makeQ [N b a])]) ~=? powersetTrans (PCon (PVar (makeQ [N b a])) [PVar (makeQ [N a b, N b a, S a b, S b a]), PVar (makeQ [N b a])], Right (Call a a))

psTest10 :: Test
psTest10 = "Check that call string finding works fine"
        ~: Just [Right (Call b a), Right (Call a b)] ~=? extractCalls (doBFS psetBA)

psTest11 :: Test
psTest11 = "Check that we find a good result for PS3"
        ~: Just [Right (Call b c), Right (Call a b), Right (Call c b)] ~=? extractCalls (doBFS psetThree)

bSATests = TestList ["Check that BFS on constructed automata is fine, for 1 call"
                    ~: Just [Right (Call a b)] ~=? (extractCalls . doBFS) solvingAutomata,
                    "Check that this is also fine for FO knowledge"
                    ~: Just [Right (Call a b)] ~=? (extractCalls . doBFS) foSA,
                    "Check that results are the same for new and old constructions"
                    ~: extractCalls (doBFS psetThree) ~=? extractCalls (doBFS saThree),
                     "Check that this doesn't error"
                     ~: Nothing ~=? extractCalls (doBFS saThreeAll)
                   ]

doBSATests = runTestTT bSATests


t1 = powersetTrans (PCon (PVar $ makeQ [N a b]) [PVar $ makeQ [N a b]], Right (Call a b))
t2 = powersetTrans (PCon (PVar $ makeQ [N b a]) [PVar $ makeQ [N b a]], Right (Call b a))

-- This won't work, as we don't have higher-order states here.
-- So we try and evaluate a knowledge formula on a PVar state, which errors (as it should do).
t3 = extractCalls . doBFS $ setSuccessfulFormula (K a (allExpertsAg [a, b])) $ solvingAutomata

t4 = extractCalls $ doBFS saThreeAll

-- It's becoming time to consider what to do for a non-permitted call
-- It seems to make the most sense to just return the empty list, thus making a
-- "non-transition" in the automata.
-- However, we need to consider what the implications of this will be elsewhere.
-- Will this be problematic for other cases
--t3 = powersetTrans (PCon (makeQ [N b a]) [makeQ [N b a], makeQ [N b a]], Right (Call b a))
--t4 = powersetTrans (PCon (makeQ [N b a]) [makeQ [N b a], makeQ [N b a], makeQ [N b a, S b c]], Right (Call b a))
t5 = powersetTrans (PCon (PVar $ makeQ [N b a, S b a, N a b, S a b]) [PVar $ makeQ [N b a, S b a, N a b, S a b]], Right (Call b a))

------------------------------------------------------------------------------

powersetTrans :: Transition (PState QState) Character
powersetTrans = transition powerset

powerset :: FSM Character (PState QState)
powerset = setSuccessfulFormula (K a (allExpertsAg [a, b])) $ psaFromScratch a model eventModel

psetBA :: FSM Character (PState QState)
psetBA = setInitial [PCon (PVar $ makeQ [N b a]) [PVar $ makeQ [N b a]]] $ setStatesReachable [PCon (PVar $ makeQ [N b a]) [PVar $ makeQ [N b a]]] powerset

solvingAutomata :: FSM Character (PState QState)
solvingAutomata = createSolvingAutomata (allExpertsAg [a, b]) model eventModel

foSA :: FSM Character (PState QState)
foSA = createSolvingAutomata (K a (allExpertsAg [a, b])) model eventModel

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

saThree :: FSM Character (PState QState)
saThree = createSolvingAutomata (K c $ allExpertsAg [a, b, c]) threeModel threeEvModel

saThreeAll :: FSM Character (PState QState)
saThreeAll = createSolvingAutomata (And [K a $ allExpertsAg [a, b, c], K b $ allExpertsAg [a, b, c], K c $ allExpertsAg [a, b, c]]) threeModel threeEvModel


psetThree :: FSM Character (PState QState)
psetThree = setStatesReachableInit $ 
            setInitial [PCon (PVar $ makeQ [N a b, N b c]) [PVar $ makeQ [N a b, N b c]]] $
            setSuccessfulFormula (K a (allExpertsAg [a, b, c])) $ 
            psaFromScratch a threeModel threeEvModel

-- Four -----------------------------------------------------

fourModel :: EpistM 
fourModel = Mo
    [State (0, [])]
    [a, b, c, d]
    [(State (0, []), [P (N a b), P (N d c), P (N d a)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]])]
    [State (0, [])]

fourEvModel :: EventModel
fourEvModel = standardEventModel [a, b, c, d] anyCall postUpdate

saFour :: FSM Character (PState QState)
saFour = createSolvingAutomata (K c $ K a $ P (S b d)) fourModel fourEvModel

st4 = FSM.transition saFour $ (head $ FSM.initial saFour, Right (Call d c))
st42 = FSM.transition saFour $ (fromJust st4, Right (Call c a))
st43 = FSM.transition saFour $ (fromJust st42, Right (Call a b))
st44 = FSM.transition saFour $ (fromJust st43, Right (Call b c))


{-
Just
    (PCon
        (PCon
            (PVar (Q (fromList [N Ag a Ag b,N Ag a Ag c,N Ag b Ag a,N Ag b Ag c,N Ag d Ag a,N Ag d Ag c,S Ag a Ag b,S Ag b Ag a])))
            [PVar (Q (fromList [N Ag a Ag b,N Ag a Ag c,N Ag b Ag a,N Ag b Ag c,N Ag d Ag a,N Ag d Ag c,S Ag a Ag b,S Ag b Ag a]))])
        [PCon
            (PVar (Q (fromList [N Ag a Ag b,N Ag a Ag c,N Ag b Ag a,N Ag b Ag c,N Ag d Ag a,N Ag d Ag c,S Ag a Ag b,S Ag b Ag a])))
            [PVar (Q (fromList [N Ag a Ag b,N Ag a Ag c,N Ag b Ag a,N Ag b Ag c,N Ag d Ag a,N Ag d Ag c,S Ag a Ag b,S Ag b Ag a]))]
        ,PCon
            (PVar (Q (fromList [N Ag a Ag b,N Ag a Ag c,N Ag a Ag d,N Ag b Ag c,N Ag d Ag a,N Ag d Ag b,N Ag d Ag c,S Ag a Ag d,S Ag d Ag a])))
            [PVar (Q (fromList [N Ag a Ag b,N Ag a Ag c,N Ag a Ag d,N Ag b Ag c,N Ag d Ag a,N Ag d Ag b,N Ag d Ag c,S Ag a Ag d,S Ag d Ag a]))]])
-}

st4' = FSM.transition saFour $ (fromMaybe undefined st4, Right (Call d b))

t_4 = extractCalls $ doBFS saFour

four_backwards = createSolvingAutomata (K a $ P (S b d)) fourModel fourEvModel

tt4 = (buildComposedSS c fourModel fourEvModel (four_backwards))

t_5 = FST.bitransition tt4 (head $ FST.initial tt4, Right (Call a b))

-- For everyone expert, Just [Right Ag c Ag d,Right Ag b Ag c,Right Ag a Ag b,Right Ag c Ag b,Right Ag d Ag c]
-- For K_a Expert,      Just [Right Ag b Ag c,Right Ag a Ag b,Right Ag b Ag d,Right Ag d Ag a,Right Ag a Ag c]
psetFour :: FSM Character (PState QState)
psetFour = setStatesReachableInit $
           setInitial [PCon (PVar $ makeQ [N a b, N b c, N c d]) [PVar $ makeQ [N a b, N b c, N c d]]] $
           setSuccessfulFormula (K a (allExpertsAg [a, b, c, d])) $
           psaFromScratch a fourModel fourEvModel

------------------------------------------------------------

-- When we do second-order knowledge, it fails. Why?


tSAFour1 = FSM.transition saFour $ (FSM.initial saFour !! 0, Right (Call a b))

twoModel :: EpistM
twoModel = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a, b, c]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]]), (b, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]

twoEvModel :: EventModel
twoEvModel = EvMo
    [Call a b, Call b a, Call a a, Call b b]
    [(a, [[Call a b], [Call b a, Call a a], [Call b b]]), (b, [[Call a b], [Call b a, Call a a], [Call b b]])]
    anyCall
    postUpdate

saTwo :: FSM Character (PState QState)
saTwo = createSolvingAutomata (K b $ K a $ allExpertsAg [a, b]) twoModel twoEvModel

saThree2 :: FSM Character (PState QState)
saThree2 = createSolvingAutomata (K a $ K b $ K a $ allExpertsAg [a, b, c]) threeModel threeEvModel




  
