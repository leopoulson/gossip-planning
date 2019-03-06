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

tev4 :: Test
tev4 = "Check that updates to everyone being an expert work "
     ~: True ~=? satisfies (updateModel, (State (0, [Call a b]))) (allExperts updateModel)

tev5 :: Test
tev5 = "Check that impossible call updates can't evaluate to true"
     ~: False ~=? satisfies (updateModel, (State (0, [Call b a]))) (P (S b a))

tev6 :: Test 
tev6 = "Indeed, check that impossible calls don't produce an updated state"
     ~: False ~=? (State (0, [Call b a])) `elem` states updateModel

tev7 :: Test 
tev7 = "Check that possible calls do yield an updated state"
     ~: True ~=? (State (0, [Call a b])) `elem` states updateModel

-- We don't need Test n in front of the actual test
tevTests :: Test
tevTests = test [tev1, tev2, tev3, tev4, tev5, tev6, tev7]

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

-- Testing Relations

-- relModel :: EpistM
-- relModel = Mo 
--     [State (0, []), State (1, [])]
--     [a, b]
--     [(State (0, []))]


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