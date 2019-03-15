module Powerset where

import Model
import FSM
import FST
import ME

import Data.List (nub)

-- data PState = PState { 
--     state :: QState,  
--     possStates :: [QState]
-- }

data PState st = PState st [st] deriving (Eq, Show)

{- There's a couple of things to sort out w.r.t this function.

 * The first is that we need some kind of guarantee that the agent whose 
   knowledge-formula we want to evaluate is the one that the current PSA
   is constructed for. This will probably require baking the agent into
   the type of either PState or the PSA itself. 
-}
instance (EvalState st, Eq st) => EvalState (PState st) where
  evalState (K _ p)  (PState _ sts) = all (evalState p) sts
  evalState f        (PState st _)  = evalState f st


buildPSA :: EvalState st => FSM Character st -> FST Character st -> FSM Character (PState st)
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

setSuccessfulFormula :: EvalState st => Form -> FSM Character (PState st) -> FSM Character (PState st)
setSuccessfulFormula f = updateAcccepting (evalState f) 




