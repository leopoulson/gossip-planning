module AutoTests where

import Verify
import MakeGraphs
import BFSM
import FSM
import Model
import ME
import Powerset

import Data.Maybe (isJust, fromJust, isNothing)
import Data.Either (rights)

type PSA = FSM Character (PState QState)

-- TODO: A lot of this could probably have been done with zip.
-- Perhaps it would be nice to update it to use this at a later date.

runTests size n = putStrLn $ prettyPrintResults $ getWithNothings size n

prec :: Precondition
prec = anyCall


prettyPrintResults :: [(Maybe [Character], Bool)] -> [Char]
prettyPrintResults ress = "Did " ++ show (length ress) ++ " tests. \n" ++ 
                          -- show (length empties) ++ " were negative results, of which " ++ show (trues empties) ++ " were True.\n" ++
                          show (length fulls)   ++ " were positive results, of which " ++ show (trues fulls) ++ " were True."
  where
    trues = length . filter (id . snd)
    empties = filter (isNothing . fst) ress
    fulls = filter (isJust . fst) ress

verifyResults :: [(EpistM, [Event])] -> [Bool]
verifyResults = map (\(ep, evs) -> verifyAllExperts ep evs)

getModelResults :: Int -> Int -> [(EpistM, [Event])]
getModelResults size n = getCalls . getJusts . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n 

getWithNothings :: Int -> Int -> [(Maybe [Character], Bool)]
getWithNothings size n = map verify . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n 

-- This function takes the size of the graphs and the count, and then generates
-- a bunch of PSAs.
getGraphs :: Int -> Int -> [PSA]
getGraphs size n = map (\(ep, ev) -> createSolvingAutomata (allExpertsAg $ getAgents size) ep ev) $ generateModels (getAgents size) prec

getCalls :: [(EpistM, [Character])] -> [(EpistM, [Event])]
getCalls = map (\(e, c) -> (e, rights c)) 

getJusts :: [(EpistM, Maybe [Character])] -> [(EpistM, [Character])]
getJusts = map (\(a, b) -> (a, fromJust b)) . filter (isJust . snd) 

verify :: (EpistM, Maybe [Character]) -> (Maybe [Character], Bool)
verify (ep, Nothing)    = (Nothing, verifyEmpty ep)
verify (ep, Just calls) = (Just calls, verifyAllExperts ep (rights calls))

getModelCalls :: [(EpistM, PSA)] -> [(EpistM, Maybe [Character])]
getModelCalls = map (\(ep, psa) -> (ep, extractCalls . doBFS $ psa))

getModelPSAPairs :: Int -> [(EpistM, EventModel)] -> [(EpistM, PSA)]
getModelPSAPairs size models = map (\(ep, ev) -> (ep, createSolvingAutomata (allExpertsAg $ getAgents size) ep ev)) models

getModels :: Int -> Int -> [(EpistM, EventModel)]
getModels size n = take n $ generateModels (getAgents size) prec

getPhonebookModels :: Int -> Int -> [(EpistM, EventModel)]
getPhonebookModels size n = take n $ generateModelsPhonebook (getAgents size) prec

mapBFS :: [PSA] -> [Maybe [Character]]
mapBFS psas = map (extractCalls . doBFS) psas 

-- This takes the number of agnets and generates the correct number of agents.
getAgents :: Int -> [Agent]
getAgents n = Ag <$> [0 .. (n - 1)]
