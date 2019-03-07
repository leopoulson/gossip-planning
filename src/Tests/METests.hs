module Tests.METests where

import ME 
import Model
import FSM
import FST
import Tests.Tests

import Test.HUnit hiding (State)

allTests :: [Test]
allTests = [meTests, transTests, idTests, cTransTests]

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
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
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

t1 :: [(Character, ((QState, QState), QState))]
t1 = biT (((Q [N a b], QInit), Q [N a b]), Right (Call a b))

t2 :: [(Character, ((QState, QState), QState))]
t2 = biT (((Q [N b a], QInit), Q [N b a]), Right (Call b a))

ctT1 :: Test
ctT1 = "Testing the way the transition works for composed transducers"
    ~: [(Right (Call a b),((Q [N a b, N b a, S a b, S b a], QInit), Q [N a b, N b a, S a b, S b a]))] ~=? t1

ctT2 :: Test
ctT2 = "Testing the way it works for indistinguishable events"
    ~: [(Right (Call b a),((Q [N a b, N b a, S a b, S b a], QInit),Q [N a b, N b a, S a b, S b a])),(Right (Call a a),((Q [N a b, N b a, S a b, S b a], QInit),Q [N b a]))] ~=? t2

cTransTests :: Test
cTransTests = TestList [ctT1, ctT2]

doCTransTests :: IO Counts
doCTransTests = runTestTT cTransTests

cTransModel :: EpistM
cTransModel = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a, b]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]]), (b, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]

cTransEv :: EventModel
cTransEv = EvMo
    [Call a b, Call b a, Call a a]
    [(a, [[Call a b], [Call b a, Call a a]]), (b, [[Call a b], [Call b a, Call a a]])]
    anyCall
    postUpdate

transAuto :: FSM Character QState
transAuto = buildDAutomata cTransModel cTransEv

cTrans :: FST Character ((QState, QState), QState)
cTrans = buildComposedTransducers a cTransModel cTransEv (buildDAutomata cTransModel cTransEv)

biT :: BiTransition ((QState, QState), QState) Character
biT = bitransition cTrans

concat1 :: FST Character (QState, QState)
concat1 = composeFST (identityTransducer transAuto) (buildTransducer a cTransModel cTransEv)

t3 = bitransition concat1 $ ((Q [N b a], QInit), Right (Call a a))

t4 :: [(Character, ((QState, QState), QState))]
t4 = biT (((Q [N b a], QInit), Q [N b a]), Right (Call a a))









