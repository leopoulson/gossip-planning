module Powerset where

import Model
import FSM
import FST
import ME

data PState = PState { 
    state :: QState,  
    possStates :: [QState],
    endStates :: QState -> [QState]
}


buildPSA :: FSM Character QState -> FST Character QState -> FSM Character PState
buildPSA fsm (FST _ _ bitransition _ _) = FSM alphabet' states' transition' initial' accepting' where
    alphabet'    = alphabet fsm
    accepting'   = accepting fsm . state 
    states'      = undefined -- hmmm what to do here? explicitly list the states? give a 'well-formed' function?
    initial'     = undefined
    transition' (PState state possStates endStates, ch) = 
        PState 
        (transition fsm (state, ch))
        (possStates' possStates ch) --[s' | s <- possStates, (_, s') <- bitransition (s, ch)] --(possStates' possStates ch)
        endStates'
    possStates' possStates ch = [s' | s <- possStates, (_, s') <- bitransition (s, ch)]
    endStates'  = undefined

