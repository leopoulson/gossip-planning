module FSM where

type Transition q e = (q, e) -> q

data FSM ch st = FSM {
    alphabet :: [ch],                -- Alphabet
    states :: [st],                  -- Set of states
    transition :: Transition st ch,  -- Transition function
    initial :: [st],                 -- Set of initial states
    accepting :: st -> Bool        -- Set of accepting states 
}