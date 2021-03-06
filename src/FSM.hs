module FSM where

import Data.Maybe (catMaybes, mapMaybe, isJust, isNothing, fromJust)
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

winningFinalStates :: Eq st => FSM ch st -> [st] -> [st]
winningFinalStates fsm sts = filter (accepting fsm) $ findPathReachable fsm sts

existsWinningPath :: Eq st => FSM ch st -> [st] -> Bool
existsWinningPath fsm sts = any (accepting fsm) $ findPathReachable fsm sts

findPathReachable :: Eq st => FSM ch st -> [st] -> [st]
findPathReachable fsm sts 
  | sts == sts' = sts
  | otherwise   = findPathReachable fsm sts'
  where
    sts' = findReachableFromSet fsm sts

-- using nub here has the same complexity as using a set then converting to a list
-- as set insertion takes O(log n), and we would insert n things
findReachableFromSet :: Eq st => FSM ch st -> [st] -> [st]
findReachableFromSet fsm sts = nub $ sts `union` concatMap (findReachableFromOne fsm) sts

findReachableFromOne :: FSM ch st -> st -> [st]
findReachableFromOne (FSM alph _ trans _ _) st = catMaybes [trans (st, ch) | ch <- alph]

updateAcccepting :: (st -> Bool) -> FSM ch st -> FSM ch st
updateAcccepting accepting' (FSM al st trans int _) = FSM al st trans int accepting' 

setStatesReachableInit :: Eq st => FSM ch st -> FSM ch st
setStatesReachableInit fsm = setStatesReachable (initial fsm) fsm

-- This function sets the states of an FSM to strictly the states that it can access 
-- Useful for FSMs with a huge possible state space
setStatesReachable :: Eq st => [st] -> FSM ch st -> FSM ch st
setStatesReachable sts fsm@(FSM al _ trans int acc) = FSM al (findReachableFromSet fsm sts) trans int acc

setInitial :: [st] -> FSM ch st -> FSM ch st
setInitial initStates (FSM al stts trans _ acc) = FSM al stts trans initStates acc
  -- | all (`elem` stts) initStates  = FSM al stts trans initStates acc
  -- | otherwise                       = error "Initial state not member of states"

findLoops :: Eq st => FSM ch st -> [(st, ch)]
findLoops fsm = [(q, e) | q <- states fsm, e <- alphabet fsm, 
                          transition fsm (q, e) == Just q]

removeLoopsFSM :: (Eq st, Eq ch) => FSM ch st -> FSM ch st
removeLoopsFSM fsm@(FSM al st trans int acc) = FSM al st trans' int acc 
  where 
    trans' = removeLoops trans $ findLoops fsm

removeLoops :: (Eq st, Eq ch) => Transition st ch -> [(st, ch)] -> Transition st ch
removeLoops = foldr removeLoop

removeLoop :: (Eq st, Eq ch) => (st, ch) -> Transition st ch -> Transition st ch
removeLoop (st, ch) trans (st', ch') 
  | (st, ch) == (st', ch')  = Nothing
  | otherwise               = trans (st', ch')

getNeighbours :: FSM ch st -> st -> [st]
getNeighbours fsm st = mapMaybe (\ch -> transition fsm (st, ch)) $ alphabet fsm 

getNeighboursEv :: FSM ch st -> st -> [(st, ch)]
getNeighboursEv fsm st = map (\(st', ch) -> (fromJust st', ch)) . 
                         filter (isJust . fst) . 
                         map (\ch -> (transition fsm (st, ch), ch)) $ alphabet fsm

unionFSM' :: [FSM ch st] -> FSM ch [st]
unionFSM' fsms = FSM alpha' states' trans' initial' accepting'
  where
    alpha' = alphabet $ head fsms
    states' = undefined
    trans' (sts, call) = undefined -- zipWith ($) (map transition fsms) $ [(st, call) | st <- sts]
    initial' = createInits $ map initial fsms
    accepting' sts = or $ zipWith ($) (map accepting fsms) sts

unionFSM :: Eq st => [FSM ch st] -> FSM ch [st]
unionFSM = intersectionFSM . map complementFSM

createInits :: [[a]]  -> [[a]]
createInits [] = [[]]
createInits (i : is) = [l : ls | l <- i, ls <- createInits is]

intersectionFSM :: Eq st => [FSM ch st] -> FSM ch [st]
intersectionFSM  fsms = setStatesReachableInit $ FSM alpha' states' trans' initial' accepting'
  where
    alpha' = alphabet $ head fsms
    states' = undefined
    trans' (sts, call) = checkMaybes $ zipWith ($) (map transition fsms) $ [(st, call) | st <- sts]
    initial' = createInits $ map initial fsms
    accepting' sts = and $ zipWith ($) (map accepting fsms) sts
    checkMaybes sts = if (any isNothing) sts then Nothing else Just $ map fromJust sts

complementFSM :: FSM ch st -> FSM ch st
complementFSM (FSM sts alph trans int accept) = FSM sts alph trans int accept'
  where
    accept' = not . accept
