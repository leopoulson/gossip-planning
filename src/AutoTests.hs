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

getIncorrectsIn :: [(EpistM State, PSA)] -> [(EpistM State, Maybe [Character])]
getIncorrectsIn pairs = filter (not . snd . verify) $ getModelCalls pairs

oddModel :: EpistM State
oddModel = Mo
  [State (0, [])]
  [a, b, c, d]
  [(State (0, []), [P (N a b), P (N c a), P (N c b), P (N c d)])]
  [(a, [[State (0, [])]]), (b, [[State (0, [])]]), (c, [[State (0, [])]]), (d, [[State (0, [])]])]
  [State (0, [])]

oddEvModel :: EventModel
oddEvModel = standardEventModel [a, b, c, d] prec postUpdate

oddPSA :: PSA
oddPSA = createSolvingAutomata (successfulFormula $ agents oddModel) oddModel oddEvModel

oddTrans = getTransducer d (buildMEStar oddModel oddEvModel)
oddTransition = FST.bitransition oddTrans

oddPSAC = createSolvingAutomata (K c (allExpertsAg [a, b, c, d])) oddModel oddEvModel
oddPSAD = createSolvingAutomata (K d (allExpertsAg [a, b, c, d])) oddModel oddEvModel

ts1 = oddTransition (fromPState $ head $ FSM.initial $ oddPSA, Right (Call c a))

s1 = FSM.transition oddPSA (head $ FSM.initial $ oddPSA, Right (Call c a))
sc1 = FSM.transition oddPSAC (head $ FSM.initial $ oddPSAC, Right (Call c a))
sd1 = FSM.transition oddPSAD (head $ FSM.initial $ oddPSAD, Right (Call c a))

s2 = FSM.transition oddPSA (fromJust s1, Right (Call a b))
sc2 = FSM.transition oddPSAC (fromJust sc1, Right (Call a b))
sd2 = FSM.transition oddPSAD (fromJust sd1, Right (Call a b))


s3 = FSM.transition oddPSA (fromJust s2, Right (Call a d))
sc3 = FSM.transition oddPSAC (fromJust sc2, Right (Call a d))
sd3 = FSM.transition oddPSAD (fromJust sd2, Right (Call a d))


s4 = FSM.transition oddPSA (fromJust s3, Right (Call b d))
sc4 = FSM.transition oddPSAC (fromJust sc3, Right (Call b d))
sd4 = FSM.transition oddPSAD (fromJust sd3, Right (Call b d))


s5 = FSM.transition oddPSA (fromJust s4, Right (Call c b))
sc5 = FSM.transition oddPSAC (fromJust sc4, Right (Call c b))
sd5 = FSM.transition oddPSAD (fromJust sd4, Right (Call c b))


getIncorrects :: Int -> Int -> [(EpistM State, Maybe [Character])]
getIncorrects size n = getIncorrectsIn . getModelPSAPairs size . getPhonebookModels size $ n

prec :: Precondition
prec = anyCall

allKnowAllExperts :: [Agent] -> Form
allKnowAllExperts ags = And $ [K ag (allExpertsAg ags) | ag <- ags]

abKnowAllExperts :: [Agent] -> Form
abKnowAllExperts ags = And [K c (allExpertsAg ags), K d (allExpertsAg ags)]

aKnowExperts :: [Agent] -> Form
aKnowExperts ags = K d (allExpertsAg ags)

dKnowExperts :: [Agent] -> Form
dKnowExperts ags = K d (allExpertsAg ags)

successfulFormula :: [Agent] -> Form
successfulFormula ags = K b (dKnowExperts ags)--K a (allKnowAllExperts ags)


prettyPrintResults :: [(Maybe [Character], Bool)] -> [Char]
prettyPrintResults ress = "Did " ++ show (length ress) ++ " tests. \n" ++ 
                          show (length empties) ++ " were negative results, of which " ++ show (trues empties) ++ " were True.\n" ++
                          show (length fulls)   ++ " were positive results, of which " ++ show (trues fulls) ++ " were True."
  where
    trues = length . filter (id . snd)
    empties = filter (isNothing . fst) ress
    fulls = filter (isJust . fst) ress

getModelResults :: Int -> Int -> [(EpistM State, [Event])]
getModelResults size n = getCalls . getJusts . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n

getWithNothings :: Int -> Int -> [(Maybe [Character], Bool)]
getWithNothings size n = map verify . getModelCalls . getModelPSAPairs size . getPhonebookModels size $ n

getCalls :: [(EpistM State, [Character])] -> [(EpistM State, [Event])]
getCalls = map (\(e, c) -> (e, rights c))

getJusts :: [(EpistM State, Maybe [Character])] -> [(EpistM State, [Character])]
getJusts = map (\(a, b) -> (a, fromJust b)) . filter (isJust . snd) 

verify :: (EpistM State, Maybe [Character]) -> (Maybe [Character], Bool)
verify (ep, Nothing)    = (Nothing, verifyEmpty ep)
verify (ep, Just calls) = (Just calls, verifyWinning ep (rights calls))

getModelCalls :: [(EpistM State, PSA)] -> [(EpistM State, Maybe [Character])]
getModelCalls = map (\(ep, psa) -> (ep, extractCalls . doBFS $ psa))

getModelPSAPairs :: Int -> [(EpistM State, EventModel)] -> [(EpistM State, PSA)]
getModelPSAPairs size models = map (\(ep, ev) -> (ep, createSolvingAutomata (successfulFormula $ getAgents size) ep ev)) models

getModels :: Int -> Int -> [(EpistM State, EventModel)]
getModels size n = take n $ generateModels (getAgents size) prec

getPhonebookModels :: Int -> Int -> [(EpistM State, EventModel)]
getPhonebookModels size n = take n $ generateModelsPhonebook (getAgents size) prec

mapBFS :: [PSA] -> [Maybe [Character]]
mapBFS psas = map (extractCalls . doBFS) psas

-- This takes the number of agnets and generates the correct number of agents.
getAgents :: Int -> [Agent]
getAgents n = Ag <$> [0 .. (n - 1)]
