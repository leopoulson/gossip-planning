module FSM where

type Transition q e = (q, e) -> q

data FSM ch st = FSM 
    [ch]                  -- Alphabet
    [st]                  -- Set of states
    (Transition st ch)    -- Transition function
    [st]                  -- Set of initial states
    [(st, Bool)]          -- Set of accepting states 