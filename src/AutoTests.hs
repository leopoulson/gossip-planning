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

import Data.Maybe (isJust, fromJust, isNothing)
import Data.Either (rights)

type PSA = FSM Character (PState QState)

-- TODO: A lot of this could probably have been done with zip.
-- Perhaps it would be nice to update it to use this at a later date.

runTests size n = putStrLn $ prettyPrintResults $ getWithNothings size n

getIncorrectsIn :: [(EpistM, PSA)] -> [(EpistM, Maybe [Character])]
getIncorrectsIn pairs = filter (not . snd . verify) $ getModelCalls pairs

oddModel :: EpistM
oddModel = Mo
  [State (0, [])]
  [a, b, c, d]
  [(State (0, []), [P (N a c), P (N b a), P (N b c), P (N b d),  P (N c a)])]
  [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]])]
  [State (0, [])]

oddEvModel :: EventModel
oddEvModel = standardEventModel [a, b, c, d] prec postUpdate

oddPSA :: PSA
oddPSA = createSolvingAutomata (successfulFormula $ agents oddModel) oddModel oddEvModel

oddCalls :: Maybe [Character]
oddCalls = Just [Right (Call a c), Right (Call b d), Right (Call b a), Right (Call d c)]

oddTrans = getTransducer d (buildMEStar oddModel oddEvModel)
oddTransition = FST.bitransition oddTrans


getIncorrects :: Int -> Int -> [(EpistM, Maybe [Character])]
getIncorrects size n = getIncorrectsIn . getModelPSAPairs size . getPhonebookModels size $ n

prec :: Precondition
prec = lns

allKnowAllExperts :: [Agent] -> Form
allKnowAllExperts ags = And $ [K ag (allExpertsAg ags) | ag <- ags]

aKnowExperts :: [Agent] -> Form
aKnowExperts ags = K a (allExpertsAg ags)

dKnowExperts :: [Agent] -> Form
dKnowExperts ags = K d (allExpertsAg ags)

successfulFormula :: [Agent] -> Form
successfulFormula = dKnowExperts

prettyPrintResults :: [(Maybe [Character], Bool)] -> [Char]
prettyPrintResults ress = "Did " ++ show (length ress) ++ " tests. \n" ++ 
                          show (length empties) ++ " were negative results, of which " ++ show (trues empties) ++ " were True.\n" ++
                          show (length fulls)   ++ " were positive results, of which " ++ show (trues fulls) ++ " were True."
  where
    trues = length . filter (id . snd)
    empties = filter (isNothing . fst) ress
    fulls = filter (isJust . fst) ress

getModelResults :: Int -> Int -> [(EpistM, [Event])]
getModelResults size n = getCalls . getJusts . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n

getWithNothings :: Int -> Int -> [(Maybe [Character], Bool)]
getWithNothings size n = map verify . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n

getCalls :: [(EpistM, [Character])] -> [(EpistM, [Event])]
getCalls = map (\(e, c) -> (e, rights c))

getJusts :: [(EpistM, Maybe [Character])] -> [(EpistM, [Character])]
getJusts = map (\(a, b) -> (a, fromJust b)) . filter (isJust . snd) 

verify :: (EpistM, Maybe [Character]) -> (Maybe [Character], Bool)
verify (ep, Nothing)    = (Nothing, verifyEmpty ep)
verify (ep, Just calls) = (Just calls, verifyWinning ep (rights calls))

getModelCalls :: [(EpistM, PSA)] -> [(EpistM, Maybe [Character])]
getModelCalls = map (\(ep, psa) -> (ep, extractCalls . doBFS $ psa))

getModelPSAPairs :: Int -> [(EpistM, EventModel)] -> [(EpistM, PSA)]
getModelPSAPairs size models = map (\(ep, ev) -> (ep, createSolvingAutomata (successfulFormula $ getAgents size) ep ev)) models

getModels :: Int -> Int -> [(EpistM, EventModel)]
getModels size n = take n $ generateModels (getAgents size) prec

getPhonebookModels :: Int -> Int -> [(EpistM, EventModel)]
getPhonebookModels size n = take n $ generateModelsPhonebook (getAgents size) prec

mapBFS :: [PSA] -> [Maybe [Character]]
mapBFS psas = map (extractCalls . doBFS) psas 

-- This takes the number of agnets and generates the correct number of agents.
getAgents :: Int -> [Agent]
getAgents n = Ag <$> [0 .. (n - 1)]
