module Tests where

import Model
import ME

import Test.HUnit hiding (State)

-- HUnit stuff

test1 :: Test
test1 = "1 + 2" ~: 3 ~=? (1 + 2)

tests :: Test
tests = TestList [TestLabel "t1" test1]





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
callEvM = EvMo [Call a b, Call b a] [(a, [[Call a b], [Call b a]]), (b, [[Call a b], [Call b a]])] anyCall postUpdate
-- satisfies (update callExample (callEvM, Call a b) (State (0, [])) (allExperts callExample)

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