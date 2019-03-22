module Main where

import BFSM
import FSM
import FST
import Gossip
import ME
import Model
import Powerset
import RS

main :: IO ()
main = putStrLn $ show $ extractCalls $ doBFS saFive

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
saThree = createSolvingAutomata (K b $ allExpertsAg [a, b, c]) threeModel threeEvModel


fourModel :: EpistM 
fourModel = Mo
    [State (0, [])]
    [a, b, c, d]
    [(State (0, []), [P (N a b), P (N b c), P (N c d)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]])]
    [State (0, [])]

fourEvModel :: EventModel
fourEvModel = standardEventModel [a, b, c, d] anyCall postUpdate

saFour :: FSM Character (PState QState)
saFour = createSolvingAutomata (K a $ allExpertsAg $ [a, b, c, d]) fourModel fourEvModel

fiveEvModel :: EventModel
fiveEvModel = standardEventModel [a, b, c, d, e] anyCall postUpdate

fiveModel :: EpistM
fiveModel = Mo
    [State (0, [])]
    [a, b, c, d, e]
    [(State (0, []), [P (N a b), P (N b c), P (N c d), P (N e d)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]])]
    [State (0, [])]

saFive :: FSM Character (PState QState)
saFive = createSolvingAutomata (K a $ allExpertsAg [a, b, c, d, e]) fiveModel fiveEvModel
  
                       
