module AutoTests where

import Verify
import MakeGraphs
import BFSM
import FSM
import Model
import ME
import Powerset

import Data.Maybe (isJust, fromJust)
import Data.Either (rights)

type PSA = FSM Character (PState QState)

-- TODO: A lot of this could probably have been done with zip.
-- Perhaps it would be nice to update it to use this at a later date.

runTests size n = prettyPrintResults $ verifyResults $ getModelResults size n

prettyPrintResults :: [Bool] -> [Char]
prettyPrintResults ress = "Did " ++ show (length ress) ++ " tests, and " ++ show (trues ress) ++ " were True."
  where
    trues = length . filter id

verifyResults :: [(EpistM, [Event])] -> [Bool]
verifyResults = map (\(ep, evs) -> verifyAllExperts ep evs)

getModelResults :: Int -> Int -> [(EpistM, [Event])]
getModelResults size n = getCalls . getJusts . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n 

-- This function takes the size of the graphs and the count, and then generates
-- a bunch of PSAs.
getGraphs :: Int -> Int -> [PSA]
getGraphs size n = map (\(ep, ev) -> createSolvingAutomata (allExpertsAg $ getAgents size) ep ev) $ generateModels (getAgents size)

getCalls :: [(EpistM, [Character])] -> [(EpistM, [Event])]
getCalls = map (\(e, c) -> (e, rights c)) 

getJusts :: [(EpistM, Maybe [Character])] -> [(EpistM, [Character])]
getJusts = map (\(a, b) -> (a, fromJust b)) . filter (isJust . snd) 

getModelCalls :: [(EpistM, PSA)] -> [(EpistM, Maybe [Character])]
getModelCalls = map (\(ep, psa) -> (ep, extractCalls . doBFS $ psa))

getModelPSAPairs :: Int -> [(EpistM, EventModel)] -> [(EpistM, PSA)]
getModelPSAPairs size models = map (\(ep, ev) -> (ep, createSolvingAutomata (allExpertsAg $ getAgents size) ep ev)) models

getModels :: Int -> Int -> [(EpistM, EventModel)]
getModels size n = take n $ generateModels $ getAgents size

getPhonebookModels :: Int -> Int -> [(EpistM, EventModel)]
getPhonebookModels size n = take n $ generateModelsPhonebook $ getAgents size

mapBFS :: [PSA] -> [Maybe [Character]]
mapBFS psas = map (extractCalls . doBFS) psas 

-- This takes the number of agnets and generates the correct number of agents.
getAgents :: Int -> [Agent]
getAgents n = Ag <$> [0 .. (n - 1)]
