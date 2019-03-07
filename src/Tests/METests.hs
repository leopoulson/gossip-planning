module Tests.METests where

import ME 
import Model
import Tests.Tests

import Test.HUnit hiding (State)

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

-- dAutomata :: FSM Character QState
-- dAUtomata = buildDAutomata exampleModel1 eventModel1