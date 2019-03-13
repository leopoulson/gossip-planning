module BFSM where

import FSM

-- can probably use a lens here !!!!!! lol
data BNode a = BNode {
    node   :: a,       -- The actual node that we're looking at
    parent :: Maybe (BNode a)  -- We might not have a predecessor yet
} deriving (Eq, Show)

bNode :: a -> BNode a
bNode a = BNode a Nothing

bNodePred :: a -> BNode a -> BNode a
bNodePred n p = BNode n (Just p)

bfs :: Eq a => FSM ch a -> [BNode a] -> [a] -> Maybe [a]
bfs fsm queue seen = case queue of 
    []     -> Nothing    -- If the queue is empty, we stop and that is that
    (q:qs) -> case (accepting fsm $ node q) of 
        True  -> Just []   -- Here construct the path 
        False -> bfs fsm (updateQueue fsm q queue seen) (seen ++ [node q]) -- Here we want to recurse 
  -- where
    -- queue' 

-- this takes an fsm, a state, queue and seen & returns new queue
updateQueue :: Eq a => FSM ch a -> BNode a -> [BNode a] -> [a] -> [BNode a]
updateQueue fsm st queue seen = enqueue queue seen bNeighbours
  where
    neighbours = getNeighbours fsm $ node st
    bNeighbours = map (`bNodePred` st) neighbours

enqueue :: Eq a => [BNode a] -> [a] -> [BNode a] -> [BNode a]
enqueue queue seen items = foldr (enqueueOne seen) queue items

-- we give this the queue, list of seen items, and the item
-- to add, the parent, and return the updated queue
enqueueOne :: Eq a => [a] -> BNode a -> [BNode a] -> [BNode a]
enqueueOne seen item queue
  | node item `elem` seen = queue
  | otherwise             = queue ++ [item]