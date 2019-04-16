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

import Test.QuickCheck

import Verify
import AutoTests

import Data.Maybe (fromJust)
import Data.Either (rights)

main :: IO ()
-- main = putStrLn $ show $ verifyAllExperts threeModel (rights $ fromJust $ threeCalls)
-- main = putStrLn $ show $ take 1 $ getIncorrects 4 100 -- runTests 4 100
main = runTests 4 5000



thesisModel :: EpistM StateC GosProp
thesisModel = standardEpistModel [a, b, c, d] $ [N a b, N a c, N a d] 

thesisEvModel :: EventModel Call GosProp
thesisEvModel = standardEventModel [a, b, c, d] anyCall postUpdate

saThesis :: FSM CallChar (PState (QState GosProp))
saThesis = createSolvingAutomata (K b $ K a (allExpertsAg [a, b, c, d])) thesisModel thesisEvModel knowFilter


threeCalls :: Maybe [Either StateC Call]
threeCalls = extractCalls $ doBFS saThree

threeModel :: EpistM StateC GosProp
threeModel = Mo
    [State (0, [])]
    [a, b, c]
    [(State (0, []), [P (N a b), P (N b c)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]])]
    [State (0, [])]
    (produceAllProps [a, b, c])

threeEvModel :: EventModel Call GosProp
threeEvModel = standardEventModel [a, b, c] anyCall postUpdate

saThree :: FSM CallChar (PState (QState GosProp))
saThree = createSolvingAutomata (allExpertsAg [a, b, c]) threeModel threeEvModel knowFilter

fourModel :: EpistM StateC GosProp
fourModel = Mo
    [State (0, [])]
    [a, b, c, d]
    [(State (0, []), [P (N a b), P (N d c), P (N d a)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]])]
    [State (0, [])]
    (produceAllProps [a, b, c, d])

fourEvModel :: EventModel Call GosProp
fourEvModel = standardEventModel [a, b, c, d] anyCall postUpdate

saFour :: FSM CallChar (PState (QState GosProp))
saFour = createSolvingAutomata (K a $ allExpertsAg [a, b, c, d]) fourModel fourEvModel knowFilter

fiveEvModel :: EventModel Call GosProp
fiveEvModel = standardEventModel [a, b, c, d, e] anyCall postUpdate

fiveModel :: EpistM StateC GosProp
fiveModel = Mo
    [State (0, [])]
    [a, b, c, d, e]
    [(State (0, []), [P (N a b), P (N b c), P (N c d), P (N e d)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]]), (e, [[State (0, [])]])]
    [State (0, [])]
    (produceAllProps [a, b, c, d, e])

saFive :: FSM CallChar (PState (QState GosProp))
saFive = createSolvingAutomata (allExpertsAg [a, b, c, d, e]) fiveModel fiveEvModel knowFilter

f :: Agent
f = Ag 6

sixEvModel :: EventModel Call GosProp
sixEvModel = standardEventModel [a, b, c, d, e, f] anyCall postUpdate

sixModel :: EpistM StateC GosProp
sixModel = Mo
    [State (0, [])]
    [a, b, c, d, e, f]
    [(State (0, []), [P (N a b), P (N b c), P (N c d), P (N e d), P (N a f)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]]), (e, [[State (0, [])]]), (f, [[State (0, [])]])]
    [State (0, [])]
    (produceAllProps [a, b, c, d, e, f])

saSix :: FSM CallChar (PState (QState GosProp))
saSix = createSolvingAutomata (allExpertsAg [a, b, c, d, e, f]) sixModel sixEvModel knowFilter


diaModel :: EpistM StateC GosProp
diaModel = Mo
    [State (0, [])]
    [a, b, c, d]
    [(State (0, []), [P (N c a), P (N c b), P (N d a), P (N d b)])]
    [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]])]
    [State (0, [])]
    (produceAllProps [a, b, c, d])

saDia :: FSM CallChar (PState (QState GosProp))
saDia = createSolvingAutomata (K d $ allExpertsAg [a, b, c, d]) diaModel diaEvModel knowFilter

allKnowAllExperts' :: [Agent] -> Form GosProp
allKnowAllExperts' ags = And $ [K ag (allExpertsAg ags) | ag <- ags]

diaEvModel :: EventModel Call GosProp
diaEvModel = standardEventModel [a, b, c, d] lns postUpdate
