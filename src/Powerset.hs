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

data PState = PState QState [QState] deriving (Eq, Show)

{- There's a couple of things to sort out w.r.t this function.

 * The first is that we need some kind of guarantee that the agent whose 
   knowledge-formula we want to evaluate is the one that the current PSA
   is constructed for. This will probably require baking the agent into
   the type of either PState or the PSA itself. 

 * The second is that we'll make PState parametric eventually. This means
   that we'll have to act differently based on the type inside it, but this
   is probably not a big problem. I suppose we have only two behaviours; 
   - if we have a PState inside, just evalPState on that
   - if we have a QState inside, evalQState on that
   ...

-}
evalPState :: Form -> PState -> Bool
evalPState (K ag p) (PState q qs) = all (evalQState p) qs
evalPState form (PState q _) = evalQState form q 

buildPSA :: FSM Character QState -> FST Character QState -> FSM Character PState
buildPSA fsm fst = FSM alphabet' states' transition' initial' accepting' where
    alphabet'                = FSM.alphabet fsm
    accepting' (PState st _) = FSM.accepting fsm st
    states'                  = undefined -- hmmm what to do here? explicitly list the states? give a 'well-formed' function?
    initial'                 = undefined
    transition' (PState state possStates, ch) = 
                 PState (FSM.transition fsm (state, ch))  -- the next "current" state
                        (getPossStates ch possStates)     -- the set of possible states we can be in
    getPossStates :: Character -> [QState] -> [QState]
    getPossStates ch = nub . concatMap (\ st -> map snd $ bitransition fst (st, ch))

psaFromScratch :: Agent -> EpistM -> EventModel -> FSM Character PState
psaFromScratch ag ep ev = buildPSA dAuto (buildComposedSS ag ep ev dAuto)
  where
    dAuto = buildDAutomata ep ev

setAccepting :: Form -> FSM Character PState -> FSM Character PState
setAccepting f (FSM alpha sts trans initialSt _) = 
  FSM alpha sts trans initialSt accepting' 
  where 
    accepting' = evalPState f







