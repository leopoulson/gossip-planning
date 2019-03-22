module BFSM where

{-# LANGUAGE BangPatterns #-}


import FSM
import Data.Maybe (mapMaybe)

import Data.List (foldl')

import Control.Applicative
import Data.Monoid
  
-- can probably use a lens here !!!!!! lol
data BNode a ch = BNode {
    node   :: !a,       -- The actual node that we're looking at
    parent :: !(Maybe (BNode a ch, ch))  -- We might not have a predecessor yet
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
enqueue queue seen items = 

  --unlist (foldl' (flip (enqueueOne seen)) (mklist queue) items) -- queue ++ filter (\item -> not $ node item `elem` seen) items

  --foldr (enqueueOne seen) queue items

newtype DList a = DList ([a] -> [a])

mklist :: [a] -> DList a
mklist xs = DList (\ys -> xs ++ ys)

unlist :: DList a -> [a]
unlist (DList dxs) = dxs []

instance Monoid (DList a) where
  mempty = DList id
  mappend (DList xs) (DList ys) = DList (xs . ys)

-- We really want to try and reduce the amount of times we make this
-- Comparison below. This is absolutely the most costly part of the program. 
enqueueOne :: Eq a => [a] -> BNode a ch -> DList (BNode a ch) -> DList (BNode a ch)
enqueueOne seen item queue
  | node item `elem` seen = queue
  | otherwise             = queue `mappend` mklist [item]

extractCalls :: Maybe [(a, Maybe ch)] -> Maybe [ch]
extractCalls list = mapMaybe snd <$> list













