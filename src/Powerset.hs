module Powerset where

import Model
import FSM
import FST
import ME
import RS
import BFSM

import Data.List (nub)
import Data.Maybe (fromMaybe)

data PState st = PCon (PState st) [PState st] | PVar st deriving (Eq, Show)

{- There's a couple of things to sort out w.r.t this function.

 * The first is that we need some kind of guarantee that the agent whose 
   knowledge-formula we want to evaluate is the one that the current PSA
   is constructed for. This will probably require baking the agent into
   the type of either PState or the PSA itself. 
-}
instance (EvalState st, Eq st) => EvalState (PState st) where
  evalState (K _ p)  (PState _ sts) = all (evalState p) sts
  evalState f        (PState st _)  = evalState f st


buildPSA :: EvalState st => FSM ch st -> FST ch st -> FSM ch (PState st)
buildPSA fsm fstr = FSM alphabet' states' transition' initial' accepting' where
    alphabet'                = FSM.alphabet fsm
    accepting' (PState st _) = FSM.accepting fsm st
    states'                  = error "No states defined" -- hmmm what to do here? explicitly list the states? give a 'well-formed' function?
    initial'                 = error "No initial states defined"
    transition' (PState state possStates, ch) = 
                case FSM.transition fsm (state, ch) of   --Probably update this to fmap eventually 
                        Just st -> Just $ PState st (getPossStates ch possStates)
                        Nothing -> Nothing 
    getPossStates ch = nub . concatMap (\ st -> map snd $ bitransition fstr (st, ch))

psaFromScratch :: Agent -> EpistM -> EventModel -> FSM Character (PState QState)
psaFromScratch ag ep ev = buildPSA dAuto (buildComposedSS ag ep ev dAuto)
  where
    dAuto = buildDAutomata ep ev

setSuccessfulFormula :: EvalState st => Form -> FSM ch st -> FSM ch st
setSuccessfulFormula f = updateAcccepting (evalState f) 

findPath :: EvalState st => Form -> RegularStructure ch st -> Maybe [ch]
findPath (K a phi) rs = undefined 
findPath phi rs       = extractCalls . doBFS $ setSuccessfulFormula phi (dAutomata rs)

-- -- lens time
buildSolveAutomata :: EvalState st => Form -> RegularStructure ch st -> RegularStructure ch (PState st)
buildSolveAutomata (K ag (K ag' phi')) rs = 
    RegularStructure (buildPSA (dAutomata $ buildSolveAutomata (K ag' phi') rs) (liftTransducer $ getTransducer ag rs))
                        undefined undefined
buildSolveAutomata (K ag phi) rs = 
    RegularStructure (buildPSA (dAutomata rs) (getTransducer ag rs)) 
                     undefined -- (liftTransducers . liftTransducers $ transducers rs) 
                     undefined --(fAutomata rs)
buildSolveAutomata phi rs        = undefined --RegularStructure (setSuccessfulFormula phi (dAutomata rs)) (transducers rs) (fAutomata rs)

liftTransducers :: [(Agent, FST ch st)] -> [(Agent, FST ch (PState st))]
liftTransducers = map (\(ag, tr) -> (ag, liftTransducer tr))

-- Lol this isn't very good. Need to think more about this
liftTransducer :: FST ch st -> FST ch (PState st)
liftTransducer (FST _ _ tr _ _) = FST undefined undefined (liftTransition tr) undefined undefined

liftTransition :: BiTransition st ch -> BiTransition (PState st) ch
liftTransition trans (PState st sts, ch) = 
    map (\(ch', s) -> (ch', PState s sts)) $ trans (st, ch)







