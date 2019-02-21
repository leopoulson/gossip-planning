module Tests where

import Model

exampleRel :: EpistM
exampleRel = Mo 
    [State (0, []), State (1, []), State (2, [])]
    [a, b, c, d]
    []
    [(a, [[State (0, []), State (1, []), State (2, [])]])]
    [State (0, [])]

exampleCallModelRel :: EventModel
exampleCallModelRel = ([Call a b, Call b c, Call c d, Call d a], [[Call a b, Call b c], [Call c d, Call d a]], anyCall, postUpdate)

exampleRelUpd :: EpistM
exampleRelUpd = update' exampleRel exampleCallModelRel

showRel :: EpistM -> [(Agent, Rel State)]
showRel (Mo _ _ _ r _) = r