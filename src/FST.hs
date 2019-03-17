module FST where

type BiTransition q e = (q, e) -> [(e, q)]
type SSTransition ch = ch -> [ch]

data FST ch st = FST {
    alphabet :: [ch],                      -- Alphabet
    states :: [st],                        -- Set of states
    bitransition :: BiTransition st ch,    -- Transition function
    initial :: [st],                       -- Set of initial states
    accepting :: st -> Bool                -- Set of accepting states 
}

data SSFST ch = SSFST {
    ssAlphabet :: [ch],                      -- Alphabet
    ssTransition :: SSTransition ch        -- Transition
}

-- This too
composeFST :: FST ch st1 -> FST ch st2 -> FST ch (st1, st2)
composeFST (FST alpha1 states1 trans1 initial1 accepting1) 
           (FST _ states2 trans2 initial2 accepting2) = 
            FST alpha  states'  trans  initial'  accepting' where
    states'  = [(s1, s2) | s1 <- states1,  s2 <- states2]
    initial' = [(s1, s2) | s1 <- initial1, s2 <- initial2]
    accepting' (s1, s2) = accepting1 s1 && accepting2 s2
    alpha = alpha1
    trans ((s1, s2), a) = [(c, (s1', s2')) | (b, s1') <- trans1 (s1, a), (c, s2') <- trans2 (s2, b)]

-- Are these even used any more
tricomposeFST :: FST ch st -> SSFST ch -> FST ch st -> FST ch st
tricomposeFST (FST alpha1 _ _ _ _) 
              (SSFST _ trans2)
              (FST _ states3 trans3 _ _) =
               FST alpha  states'  trans  initial'  accepting'   where
    alpha = alpha1 
    states' = states3
    initial' = undefined
    accepting' = undefined
    trans (s1, a) = [(c, s2) | b <- trans2 a, (c, s2) <- trans3 (s1, b)]
    -- trans (s1, a) = [(d, s3) | (b, s2) <- trans1 (s1, a), c <- trans2 b, (d, s3) <- trans3 (s2, c)]

composeSS :: SSFST ch -> FST ch st -> FST ch st
composeSS (SSFST _ transSS) (FST alpha statesT trans initialT acceptingT) =
          FST alpha' states' trans' initial' accepting' where
    alpha' = alpha
    states' = statesT
    initial' = initialT
    accepting' = acceptingT
    trans' (s1, a) = [(c, s2) | b <- transSS a, (c, s2) <- trans (s1, b)]

-- So we take the value returned from t1 and put this into s2 

getWordTrans :: FST ch st -> st -> [ch] -> [[ch]]
getWordTrans trs int cs = map fst <$> startWordTrans trs int cs

startWordTrans :: FST ch st -> st -> [ch] -> [[(ch, st)]]
startWordTrans trs initState (c : cs) = wordTrans trs cs [[(call, state)] | (call, state) <- bitransition trs (initState, c) ]
startWordTrans _ _ [] = []

wordTrans :: FST ch st -> [ch] -> [[(ch, st)]] -> [[(ch, st)]]
wordTrans _ [] ws = ws
wordTrans trs (c:cs) ws = wordTrans trs cs $ concatMap (manyMove c) ws 
  where
    relCalls call state = bitransition trs (state, call)
    -- manyMove :: ch -> [(ch, st)] -> [[(ch, st)]]
    manyMove call word = [word ++ [(call', state')] | (call', state') <- relCalls call (snd . last $ word)]
-- wordTrans trs []






