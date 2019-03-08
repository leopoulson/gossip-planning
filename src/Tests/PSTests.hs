module Tests.PSTests where

import Model
import FSM
import FST 
import SSFST
import ME 
import Powerset

-- psTest1 :: Test
-- psTest1 = "Test that transition for non-related states is correct"
--        ~:

powersetTrans :: Transition PState Character
powersetTrans = transition powerset

powerset :: FSM Character PState
powerset = psaFromScratch a model eventModel

model :: EpistM
model = Mo
    [State (0, []), State (1, []), State (2, [])]
    [a, b]
    [(State (0, []), [P (N a b)]), (State (1, []), [P (N a b)]), (State (2, []), [P (N b a)])]
    [(a, [[State (0, [])], [State (1, []), State (2, [])]]), (b, [[State (0, [])], [State (1, []), State (2, [])]])]
    [State (0, [])]

eventModel :: EventModel
eventModel = EvMo
    [Call a b, Call b a, Call a a]
    [(a, [[Call a b], [Call b a, Call a a]]), (b, [[Call a b], [Call b a, Call a a]])]
    anyCall
    postUpdate

