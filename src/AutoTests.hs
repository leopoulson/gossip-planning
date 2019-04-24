{-# LANGUAGE FlexibleInstances #-}

module AutoTests where

import Verify
import MakeGraphs
import BFSM
import FSM
import FST
import Model
import ME
import Powerset
import RS

import Test.QuickCheck

import Data.Maybe (isJust, fromJust, isNothing)
import Data.Either (rights)

-- TODO: A lot of this could probably have been done with zip.
-- Perhaps it wouldbe nice to update it to use this at a later date.

runTests size n = putStrLn $ prettyPrintResults $ getWithNothings size n

getIncorrectsIn :: [(EpistM StateC GosProp, PSA Call GosProp)] -> [(EpistM StateC GosProp, Maybe [CallChar])]
getIncorrectsIn pairs = filter (not . snd . verify) $ getModelCalls pairs

oddModel :: EpistM StateC GosProp 
oddModel = standardEpistModel [a, b, c, d] [N a c, N b a, N b c, N b d, N d c, N d b]

oddEvModel :: EventModel Call GosProp
oddEvModel = standardEventModel [a, b, c, d] prec postUpdate

oddPSA :: PSA Call GosProp
oddPSA = createSolvingAutomata (successfulFormula $ agents oddModel) oddModel oddEvModel knowFilter

trans = FSM.transition oddPSA

-- t1 = trans (oddinit, Right (Call b a))
-- t2 = trans (fromJust t1, Right (Call a c))
-- t3 = trans (fromJust t2, Right (Call a d))
-- t4 = trans (fromJust t3, Right (Call b c))
-- t5 = trans (fromJust t4, Right (Call b d))
-- t6 = trans (fromJust t5, Right (Call c d))

t1 = trans (oddinit, Right (Call b a))
t2 = trans (fromJust t1, Right (Call a c))
t3 = trans (fromJust t2, Right (Call c d))
t4 = trans (fromJust t3, Right (Call a d))
t5 = trans (fromJust t4, Right (Call b c))

-- So this sequence of calls is successful; but it's not finding it. 


oddinit = head $ FSM.initial oddPSA

getIncorrects :: Int -> Int -> [(EpistM StateC GosProp, Maybe [CallChar])]
getIncorrects size n = getIncorrectsIn . getModelPSAPairs size . getPhonebookModels size $ n

prec :: Precondition Call GosProp
prec = lns

allKnowAllExperts :: [Agent] -> Form GosProp
allKnowAllExperts ags = And $ [K ag (allExpertsAg ags) | ag <- ags]

abKnowAllExperts :: [Agent] -> Form GosProp
abKnowAllExperts ags = And [K a (allExpertsAg ags), K b (allExpertsAg ags)]

aKnowExperts :: [Agent] -> Form GosProp
aKnowExperts ags = K a (allExpertsAg ags)

dKnowExperts :: [Agent] -> Form GosProp
dKnowExperts ags = K d (allExpertsAg ags)

successfulFormula :: [Agent] -> Form GosProp
successfulFormula = allKnowAllExperts


prettyPrintResults :: [(Maybe [CallChar], Bool)] -> [Char]
prettyPrintResults ress = "Did " ++ show (length ress) ++ " tests. \n" ++ 
                          show (length empties) ++ " were negative results, of which " ++ show (trues empties) ++ " were True.\n" ++
                          show (length fulls)   ++ " were positive results, of which " ++ show (trues fulls) ++ " were True."
  where
    trues = length . filter (id . snd)
    empties = filter (isNothing . fst) ress
    fulls = filter (isJust . fst) ress

getModelResults :: Int -> Int -> [(EpistM StateC GosProp, [Call])]
getModelResults size n = getCalls . getJusts . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n

getWithNothings :: Int -> Int -> [(Maybe [CallChar], Bool)]
getWithNothings size n = map verify . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n

getCalls :: [(EpistM StateC GosProp, [CallChar])] -> [(EpistM StateC GosProp, [Call])]
getCalls = map (\(e, c) -> (e, rights c))

getJusts :: [(EpistM StateC GosProp, Maybe [CallChar])] -> [(EpistM StateC GosProp, [CallChar])]
getJusts = map (\(a, b) -> (a, fromJust b)) . filter (isJust . snd) 

verify :: (EpistM StateC GosProp, Maybe [CallChar]) -> (Maybe [CallChar], Bool)
verify (ep, Nothing)    = (Nothing, verifyEmpty ep)
verify (ep, Just calls) = (Just calls, verifyWinning ep (rights calls))

getModelCalls :: [(EpistM StateC GosProp, PSA Call GosProp)] -> [(EpistM StateC GosProp, Maybe [CallChar])]
getModelCalls = map (\(ep, psa) -> (ep, extractCalls . doBFS $ psa))

getModelPSAPairs :: Int -> [(EpistM StateC GosProp, EventModel Call GosProp)] -> [(EpistM StateC GosProp, PSA Call GosProp)]
getModelPSAPairs size models = map (\(ep, ev) -> (ep, createSolvingAutomata (successfulFormula $ getAgents size) ep ev knowFilter)) models

getModels :: Int -> Int -> [(EpistM StateC GosProp, EventModel Call GosProp)]
getModels size n = take n $ generateModels (getAgents size) prec

getPhonebookModels :: Int -> Int -> [(EpistM StateC GosProp, EventModel Call GosProp)]
getPhonebookModels size n = take n $ generateModelsPhonebook (getAgents size) prec

mapBFS :: (Show p, Ord p) => [PSA Call p] -> [Maybe [CallChar]]
mapBFS psas = map (extractCalls . doBFS) psas

-- QuickCheck stuff

getPSA :: EpistM StateC GosProp -> PSA Call GosProp
getPSA mo = createSolvingAutomata (successfulFormula $ agents mo) mo (standardEventModel (agents mo) prec postUpdate) knowFilter

getSolution :: PSA Call GosProp -> Maybe [CallChar]
getSolution = extractCalls . doBFS

qcfn :: EpistM StateC GosProp -> Bool
qcfn model = snd $ verify (model, getSolution $ getPSA model)
