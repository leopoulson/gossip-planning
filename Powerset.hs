module Powerset where

import Model
import FSM
import ME

data PState = PState { 
    state :: QState,  
    possStates :: [QState],
    endStates :: QState -> [QState]
}


buildPSA :: FSM Character' QState -> FSM Character' PState
buildPSA fsm = FSM alphabet' states' transition' initial' accepting' where
    alphabet'    = alphabet fsm
    accepting'   = accepting fsm . state 
    states'      = undefined -- hmmm what to do here? explicitly list the states? give a 'well-formed' function?
    initial'     = undefined
    transition' (ps, ch) = PState state' possStates' endStates'
    state'       = undefined 
    possStates'  = undefined
    endStates'   = undefined

