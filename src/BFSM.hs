module BFSM where

import FSM
import Data.Maybe (mapMaybe)

-- can probably use a lens here !!!!!! lol
data BNode a ch = BNode {
    node   :: a,       -- The actual node that we're looking at
    parent :: Maybe (BNode a ch, ch)  -- We might not have a predecessor yet
} deriving (Eq, Show)

bNode :: a -> BNode a ch
bNode a = BNode a Nothing

bNodePred :: a -> BNode a ch -> ch -> BNode a ch
bNodePred n p ch = BNode n (Just (p, ch))

doBFS :: Eq a => FSM ch a -> Maybe [(a, Maybe ch)]
doBFS fsm = reverse <$> bfs fsm (map bNode $ initial fsm) []

bfs :: Eq a => FSM ch a -> [BNode a ch] -> [a] -> Maybe [(a, Maybe ch)]
bfs fsm queue seen = case queue of 
    []     -> Nothing    -- If the queue is empty, we stop and that is that
    (q:qs) -> case accepting fsm $ node q of 
        True  -> Just $ rebuildPath q                                     -- Here construct the path 
        False -> bfs fsm (updateQueue fsm q qs seen) (seen ++ [node q])   -- Here we want to recurse 

rebuildPath :: BNode a ch -> [(a, Maybe ch)]
rebuildPath (BNode a (Just (bn, ch))) = (a, Just ch) : rebuildPath bn
rebuildPath (BNode a Nothing)   = [(a, Nothing)]

-- this takes an fsm, a state, queue and seen & returns new queue
updateQueue :: Eq a => FSM ch a -> BNode a ch -> [BNode a ch] -> [a] -> [BNode a ch]
updateQueue fsm st queue seen = enqueue queue seen bNeighbours
  where
    neighbours = getNeighboursEv fsm $ node st
    bNeighbours = map (\(st', ch) -> BNode st' (Just (st, ch))) neighbours

enqueue :: Eq a => [BNode a ch] -> [a] -> [BNode a ch] -> [BNode a ch]
enqueue queue seen items = foldr (enqueueOne seen) queue items

-- we give this the queue, list of seen items, and the item
-- to add, the parent, and return the updated queue
enqueueOne :: Eq a => [a] -> BNode a ch -> [BNode a ch] -> [BNode a ch]
enqueueOne seen item queue
  | node item `elem` seen = queue
  | otherwise             = queue ++ [item]

extractCalls :: Maybe [(a, Maybe ch)] -> Maybe [ch]
extractCalls list = mapMaybe snd <$> list













