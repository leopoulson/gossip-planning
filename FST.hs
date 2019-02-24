module FST where

type BiTransition q e = (q, e) -> [(e, q)]

data FST ch st = FST 
    [ch]                    -- Alphabet
    [st]                    -- Set of states
    (BiTransition st ch)    -- Transition function
    [st]                    -- Set of initial states
    [(st, Bool)]            -- Set of accepting states 

composeFST :: FST ch st1 -> FST ch st2 -> FST ch (st1, st2)
composeFST (FST alpha1 states1 trans1 initial1 accepting1) 
           (FST alpha2 states2 trans2 initial2 accepting2) = 
            FST alpha  states  trans  initial  accepting where
    states  = [(s1, s2) | s1 <- states1,  s2 <- states2]
    initial = [(s1, s2) | s1 <- initial1, s2 <- initial2]
    accepting = [((s1, s2), b1 && b2) | (s1, b1) <- accepting1, (s2, b2) <- accepting2]
    alpha = alpha1
    trans ((s1, s2), a) = [(c, (s1', s2')) | (b, s1') <- trans1 (s1, a), (c, s2') <- trans2 (s2, b)]










