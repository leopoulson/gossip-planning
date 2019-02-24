module FST where

type BiTransition q e = (q, e) -> [(e, q)]

data FST ch st = FST 
    [ch]                    -- Alphabet
    [st]                    -- Set of states
    (BiTransition st ch)    -- Transition function
    [st]                    -- Set of initial states
    [(st, Bool)]            -- Set of accepting states 