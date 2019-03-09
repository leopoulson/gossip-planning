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

buildPSA :: FSM Character QState -> FST Character QState -> FSM Character PState
buildPSA fsm fst = FSM alphabet' states' transition' initial' accepting' where
    alphabet'    = FSM.alphabet fsm
    accepting'   = undefined --FSM.accepting fsm . state 
    states'      = undefined -- hmmm what to do here? explicitly list the states? give a 'well-formed' function?
    initial'     = undefined
    transition' (PState state possStates, ch) = 
                 PState (FSM.transition fsm (state, ch))  -- the next "current" state
                        (getPossStates ch possStates)     -- the set of possible states we can be in
    getPossStates :: Character -> [QState] -> [QState]
    getPossStates ch = nub . concatMap (\ st -> map snd $ bitransition fst (st, ch))

psaFromScratch :: Agent -> EpistM -> EventModel -> FSM Character PState
psaFromScratch ag ep ev = buildPSA dAuto (buildComposedSS ag ep ev dAuto)
  where
    dAuto = buildDAutomata ep ev







