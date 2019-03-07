module FST where

import SSFST

type BiTransition q e = (q, e) -> [(e, q)]

data FST ch st = FST {
    alphabet :: [ch],                      -- Alphabet
    states :: [st],                        -- Set of states
    bitransition :: BiTransition st ch,    -- Transition function
    initial :: [st],                       -- Set of initial states
    accepting :: st -> Bool                -- Set of accepting states 
}

composeFST :: FST ch st1 ->FST ch st2 -> FST ch (st1, st2)
composeFST (FST alpha1 states1 trans1 initial1 accepting1) 
           (FST alpha2 states2 trans2 initial2 accepting2) = 
            FST alpha  states  trans  initial  accepting where
    states  = [(s1, s2) | s1 <- states1,  s2 <- states2]
    initial = [(s1, s2) | s1 <- initial1, s2 <- initial2]
    accepting (s1, s2) = accepting1 s1 && accepting2 s2
    alpha = alpha1
    trans ((s1, s2), a) = [(c, (s1', s2')) | (b, s1') <- trans1 (s1, a), (c, s2') <- trans2 (s2, b)]


tricomposeFST :: FST ch st -> SSFST ch -> FST ch st -> FST ch st
tricomposeFST (FST alpha1 states1 trans1 initial1 accepting1) 
              (SSFST alpha2 trans2)
              (FST alpha3 states3 trans3 initial3 accepting3) =
               FST alpha  states  trans  initial  accepting   where
    alpha = alpha1 
    states = states3
    initial = undefined
    accepting = undefined
    trans (s1, a) = [(d, s3) | (b, s2) <- trans1 (s1, a), c <- trans2 b, (d, s3) <- trans3 (s2, c)]

-- So we take the value returned from t1 and put this into s2 









