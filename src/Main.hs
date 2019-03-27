module Main where

import BFSM
import FSM
import FST
import Gossip
import ME
import Model
import Powerset
import RS
import MakeGraphs

import Verify

main :: IO ()
main = putStrLn $ show $ t

--putStrLn $ show $ extractCalls $ doBFS saThree

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
saThree = createSolvingAutomata (K a $ allExpertsAg [a, b, c]) threeModel threeEvModel


fourModel :: EpistM 
fourModel = Mo
    [State (0, [])]
    [a, b, c, d]
    [(State (0, []), [P (N a b), P (N d c), P (N d a)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]])]
    [State (0, [])]

fourEvModel :: EventModel
fourEvModel = standardEventModel [a, b, c, d] anyCall postUpdate

saFour :: FSM Character (PState QState)
saFour = createSolvingAutomata (K a $ allExpertsAg [a, b, c, d]) fourModel fourEvModel

fiveEvModel :: EventModel
fiveEvModel = standardEventModel [a, b, c, d, e] anyCall postUpdate

fiveModel :: EpistM
fiveModel = Mo
    [State (0, [])]
    [a, b, c, d, e]
    [(State (0, []), [P (N a b), P (N b c), P (N c d), P (N e d)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]]), (e, [[State (0, [])]])]
    [State (0, [])]

saFive :: FSM Character (PState QState)
saFive = createSolvingAutomata (allExpertsAg [a, b, c, d, e]) fiveModel fiveEvModel

f :: Agent
f = Ag 6

sixEvModel :: EventModel
sixEvModel = standardEventModel [a, b, c, d, e, f] anyCall postUpdate

sixModel :: EpistM
sixModel = Mo
    [State (0, [])]
    [a, b, c, d, e, f]
    [(State (0, []), [P (N a b), P (N b c), P (N c d), P (N e d), P (N a f)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]]), (e, [[State (0, [])]]), (f, [[State (0, [])]])]
    [State (0, [])]

saSix :: FSM Character (PState QState)
saSix = createSolvingAutomata (allExpertsAg [a, b, c, d, e, f]) sixModel sixEvModel
