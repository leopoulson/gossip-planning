module FSM where

import Data.Maybe (catMaybes)
import Data.List (nub, union)

-- Basically, this is going to have to change to be Maybe q
-- What a nightmare 
type Transition q e = (q, e) -> Maybe q

data FSM ch st = FSM {
    alphabet :: [ch],                -- Alphabet
    states :: [st],                  -- Set of states
    transition :: Transition st ch,  -- Transition function
    initial :: [st],                 -- Set of initial states
    accepting :: st -> Bool        -- Set of accepting states 
}

findPathReachable :: Eq st => FSM ch st -> [st] -> [st]
findPathReachable fsm sts 
  | sts == sts' = sts
  | otherwise   = findPathReachable fsm sts'
  where
    sts' = findReachableFromSet fsm sts

findReachableFromSet :: Eq st => FSM ch st -> [st] -> [st]
findReachableFromSet fsm sts = nub $ sts `union` concatMap (findReachableFromOne fsm) sts

findReachableFromOne :: FSM ch st -> st -> [st]
findReachableFromOne (FSM alph _ trans _ _) st = catMaybes [trans (st, ch) | ch <- alph]








