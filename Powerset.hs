module Powerset where

import Model
import FSM
import ME

data PState = PState { 
    state :: QState,  
    possStates :: [QState],
    endStates :: QState -> [QState]}


buildPSA :: FSM Character' QState -> FSM Character' PState
buildPSA fsm = FSM alphabet' states' transition' initial' accepting' where
    alphabet'    = FSM.alphabet fsm
    states'      = undefined -- hmmm what to do here? explicitly list the states?
    transition'  = undefined
    initial'     = undefined
    accepting'   = \ps -> accepting fsm $ state ps  


