module Tests.METests where

import ME 
import Model
import FSM
import FST
import Tests.Tests

import Test.HUnit hiding (State)

allTests :: [Test]
allTests = [meTests, transTests, idTests]

doAllTests :: IO Counts
doAllTests = runTestTT $ concatTests allTests

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

-- Tests for one-state transducer construction

transT1 :: Test
transT1 = "Check that transducer handles reflexive relations"
       ~: [(Right (Call a b), QInit)] ~=?  bitransition trans (QInit, Right (Call a b))

transT2 :: Test
transT2 = "Check that transducer handles relations to other items"
      ~: [(Right (Call b a), QInit), (Right (Call a a), QInit)] ~=? bitransition trans (QInit, Right (Call a a))

transT3 :: Test
transT3 = "Check that transducer handles relations to other items"
      ~: [(Right (Call b a), QInit), (Right (Call a a), QInit)] ~=? bitransition trans (QInit, Right (Call b a))

transT4 :: Test
transT4 = "Check as above, but for worlds"
     ~: [(Left (State (0, [])), QInit)] ~=? bitransition trans (QInit, Left (State (0, [])))

transT5 :: Test
transT5 = "Relations btwn worlds"
      ~: [(Left (State (1, [])), QInit), (Left (State (2, [])), QInit)]  ~=? bitransition trans (QInit, Left (State (1, [])))

transTests :: Test
transTests = TestList [transT1, transT2, transT3, transT4, transT5]

doTransTests :: IO Counts
doTransTests = runTestTT transTests

transModel :: EpistM
transModel = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a]
    []
    [(a, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]

transEv :: EventModel
transEv = EvMo
    [Call a b, Call b a, Call a a]
    [(a, [[Call a b], [Call b a, Call a a]])]
    anyCall
    postUpdate

trans :: FST Character QState
trans = buildTransducer a transModel transEv

-- Tests for identity transducer construction

idTT1 :: Test
idTT1 = "Check id Transducer working correctly from QInit"
     ~: [(Left (State (0, [])), Q [S a b, N a b])] ~=? bitransition idT (QInit, Left (State (0, [])))

idTT2 :: Test
idTT2 = "Check id Transducer working fine for some call"
     ~: [(Right (Call a b), Q [S a b, S b a, N a b, N b a])] ~=? bitransition idT (Q [S a b, N a b], Right (Call a b)) 

idTests :: Test
idTests = TestList [idTT1, idTT2]

doIdTests :: IO Counts
doIdTests = runTestTT idTests

idT :: FST Character QState
idT = identityTransducer dAutomata

-- Tests for composition transducer construction







