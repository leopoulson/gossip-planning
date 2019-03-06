module ModelTests where

import Model

import Test.HUnit hiding (State)

-- Testing Evaluation

tev1 :: Test
tev1 = "Check evaluation works for truth " 
     ~: True ~=? satisfies (exampleModel1, (State (0, []))) (P (N a b))

tev2 :: Test
tev2 = "Check evaluation works for false " 
     ~: False ~=? satisfies (exampleModel1, (State (0, []))) (P (S b a))

tev3 :: Test
tev3 = "Check updates working"
     ~: True ~=? satisfies (updateModel, (State (0, [Call a b]))) (P (S b a))


-- We don't need Test n in front of the actual test
tevTests :: Test
tevTests = test ["Test 1" ~: tev1, 
                 "Test 2" ~: tev2,
                 "Test 3" ~: tev3]

exampleModel1 :: EpistM
exampleModel1 = Mo 
    [State (0, [])]
    [a, b]
    [(State (0, []), [P (S a b), P (N a b)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]])]
    [State (0, [])]

eventModel1 :: EventModel
eventModel1 = EvMo 
    [Call a b, Call b a] 
    [(a, [[Call a b], [Call b a]]), (b, [[Call a b], [Call b a]])] 
    anyCall 
    postUpdate

updateModel :: EpistM
updateModel = update exampleModel1 eventModel1


-- Testing Calls

example :: EpistM
example = Mo 
    [State (0, []), State (1, []), State (2, []), State (3, [])]
    [a, b, c]
    [(State (0, []), [P (S a b)]), (State (1, []), [P (S a b)]), (State (2, []), [P (S a b)])]
    [(a, [[State (0, [])], [State (1, [])], [State (2, [])], [State (3, [])]]), (b, [[State (0, [])], [State (1, [])], [State (2, [])], [State (3, [])]]), (c, [[State (0, []), State (1, []), State (2, [])], [State (3, [])]])]
    [State (1, [])]

callExample :: EpistM
callExample = Mo 
    [State (0, [])] 
    [a, b] 
    [(State (0, []), [P (N a b), P (N a a), P (N b b), P (S a a), P (S b b)])] 
    [(a, [[State (0, [])]]), (b, [[State (0, [])]])] 
    [State (1, [])]

callEvM :: EventModel
callEvM = EvMo 
    [Call a b, Call b a] 
    [(a, [[Call a b], [Call b a]]), (b, [[Call a b], [Call b a]])] 
    anyCall 
    postUpdate
-- satisfies (update callExample callEvM, (State (0, []))) (allExperts callExample)

-- Testing Relations
exampleRel :: EpistM
exampleRel = Mo 
    [State (0, []), State (1, []), State (2, [])]
    [a, b, c, d]
    []
    [(a, [[State (0, []), State (1, []), State (2, [])]])]
    [State (0, [])]

exampleCallModelRel :: EventModel
exampleCallModelRel = EvMo [Call a b, Call b c, Call c d, Call d a] [(a, [[Call a b, Call b c], [Call c d, Call d a]])] anyCall postUpdate

exampleRelUpd :: EpistM
exampleRelUpd = update exampleRel exampleCallModelRel

showRel :: EpistM -> [(Agent, Rel State)]
showRel (Mo _ _ _ r _) = r