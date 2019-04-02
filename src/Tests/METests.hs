module Tests.METests where

import ME 
import Model
import FSM
import FST
import Tests.Tests

import Data.Set (Set, fromList)

import Test.HUnit hiding (State)

type GosState = QState GosProp

allTests :: [Test]
allTests = [meTests, transTests, idTests, ssTests]

doAllTests :: IO Counts
doAllTests = runTestTT $ concatTests allTests

-- Tests for ME* construction

tStates :: Test
tStates = "Check that the states are enumerated correctly"
      ~: getQStates exampleModel ~=? FSM.states dAutomata

tTrans1 :: Test
tTrans1 = "Check that transition is working as it should, from initial state"
       ~: Just (Q (fromList ([S a b, N a b]))) ~=? transition dAutomata (QInit, Left (State (0, []))) 

tTrans2 :: Test
tTrans2 = "Test transition updated by call"
       ~: Just (Q (fromList [S a b, S b a, N a b, N b a])) ~=? transition dAutomata (Q (fromList [S a b, N a b]), Right (Call a b))

tTrans3 :: Test
tTrans3 = "Test transition updated by call when everyone knows everything"
       ~: Just (Q (fromList [S a b, S b a, N a b, N b a])) ~=? transition dAutomata (Q (fromList [S a b, S b a, N a b, N b a]), Right (Call a b))

meTests :: Test
meTests = test [tStates, tTrans1, tTrans2, tTrans3]

domeTest :: IO Counts
domeTest = runTestTT meTests

exampleModel :: EpistM StateC GosProp
exampleModel = Mo 
    [State (0, [])]
    [a, b]
    [(State (0, []), [P (S a b), P (N a b)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]])]
    [State (0, [])]
    (produceAllProps [a, b])

eventModel :: EventModel Call GosProp
eventModel = EvMo 
    [Call a b, Call b a] 
    [(a, [[Call a b], [Call b a]]), (b, [[Call a b], [Call b a]])] 
    anyCall 
    postUpdate

relUpdate :: EpistM StateC GosProp
relUpdate = update exampleModel eventModel

dAutomata :: FSM Character (QState GosProp)
dAutomata = buildDAutomataNoF exampleModel eventModel

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

transModel :: EpistM StateC GosProp
transModel = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]
    (produceAllProps [a])

transEv :: EventModel Call GosProp
transEv = EvMo
    [Call a b, Call b a, Call a a]
    [(a, [[Call a b], [Call b a, Call a a]])]
    anyCall
    postUpdate

trans :: FST Character (QState GosProp)
trans = buildTransducer a transModel transEv

-- Tests for identity transducer construction

idTT1 :: Test
idTT1 = "Check id Transducer working correctly from QInit"
     ~: [(Left (State (0, [])), Q (fromList [S a b, N a b]))] ~=? bitransition idT (QInit, Left (State (0, [])))

idTT2 :: Test
idTT2 = "Check id Transducer working fine for some call"
     ~: [(Right (Call a b), Q (fromList [S a b, S b a, N a b, N b a]))] ~=? bitransition idT (Q (fromList [S a b, N a b]), Right (Call a b)) 

idTests :: Test
idTests = TestList [idTT1, idTT2]

doIdTests :: IO Counts
doIdTests = runTestTT idTests

idT :: FST Character (QState GosProp)
idT = identityTransducer dAutomata

-- Tests for composition transducer construction

cTransModel :: EpistM StateC GosProp
cTransModel = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a, b]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]]), (b, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]
    (produceAllProps [a, b])

cTransEv :: EventModel Call GosProp
cTransEv = EvMo
    [Call a b, Call b a, Call a a]
    [(a, [[Call a b], [Call b a, Call a a]]), (b, [[Call a b], [Call b a, Call a a]])]
    anyCall
    postUpdate

-- Testing SSFST

-- Our problem is essentially that we want our composed transducer to relate calls, and tell us which 
-- state we could be in having just consumed that input at our current one. However this is not working.

-- It seems that the identity transducer contains in itself the call update, thus updating our state
-- with an event before we can actually process the event. This is not particularly effective, and does 
-- not solve our problem. 
-- This could perhaps be rectified by removing the transducer that comes before the identity transducer,
-- which would stop the computation from happening. Further, it would mean that we get our set of 
-- indistinguishable events and *then* act upon them. This seems like it's exactly what we actually 
-- want to do. Putting the transducer in front never reallly made that much sense in the first place anyway...

-- With this change, it now behaves like we would expect it to. However, it's important to consider if this
-- is really the behaviour that we want?

ssTest1 :: Test
ssTest1 = "Check that for single relations, behaves as expected"
       ~: [(Right (Call a b), Q (fromList [S a b, S b a, N a b, N b a]))] ~=? tripleTransFn (Q (fromList [N a b]), Right (Call a b))

ssTest2 :: Test
ssTest2 = "Checl that it also works for non-single ones"
       ~: [(Right (Call b a), Q (fromList [S a b, S b a, N a b, N b a])), (Right (Call a a), Q (fromList [N b a]))] ~=? tripleTransFn (Q (fromList [N b a]), Right (Call b a))     

ssTest3 :: Test
ssTest3 = "Check that it's the same for two indistinguishable calls"
       ~: tripleTransFn (Q (fromList [N b a]), Right (Call b a)) ~=? tripleTransFn (Q (fromList [N b a]), Right (Call a a))

ssTests :: Test
ssTests = TestList [ssTest1, ssTest2, ssTest3]

dossTests :: IO Counts
dossTests = runTestTT ssTests

sstrans :: SSFST Character
sstrans = buildSSTransducer a cTransModel cTransEv

idTrans :: FST Character GosState
idTrans = identityTransducer transAuto

transAuto :: FSM Character GosState
transAuto = buildDAutomataNoF cTransModel cTransEv

tripleTrans :: FST Character GosState
tripleTrans = buildComposedSS a cTransModel cTransEv transAuto

tripleTransFn :: BiTransition GosState Character
tripleTransFn = bitransition tripleTrans

ttTest1 :: [(Character, GosState)]
ttTest1 = tripleTransFn (Q $ fromList [N a b], Right (Call a b))

ttTest2 :: [(Character, GosState)]
ttTest2 = tripleTransFn (Q $ fromList [N b a], Right (Call b a))

ttTest3 :: [(Character, GosState)]
ttTest3 = tripleTransFn (Q $ fromList [N b a], Right (Call a a))









