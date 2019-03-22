module BFSM where

{-# LANGUAGE BangPatterns #-}


import FSM
import Data.Maybe (mapMaybe)

import Data.List (foldl')
import Data.Set (Set, empty, insert, notMember)

import Control.Applicative hiding (empty)
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

doBFS :: Ord a => FSM ch a -> Maybe [(a, Maybe ch)]
doBFS fsm = bfs fsm (map bNode $ initial fsm) empty 

bfs :: Ord a => FSM ch a -> [BNode a ch] -> Set a -> Maybe [(a, Maybe ch)]
bfs fsm queue seen = case queue of 
    []     -> Nothing    -- If the queue is empty, we stop and that is that
    (q:qs) -> case accepting fsm $ node q of 
        True  -> Just $ rebuildPath q                                     -- Here construct the path 
        False -> bfs fsm (updateQueue fsm q qs seen) (insert (node q) seen)   -- Here we want to recurse 

--rebuildPath :: BNode a ch -> [(a, Maybe ch)]
--rebuildPath (BNode a (Just (bn, ch))) = (a, Just ch) : rebuildPath bn
--rebuildPath (BNode a Nothing)   = [(a, Nothing)]

rebuildPath :: BNode a ch -> [(a, Maybe ch)]
rebuildPath = go []
  where
    go acc (BNode x Nothing)         = (x, Nothing):acc
    go acc (BNode x (Just (bn, ch))) = go ((x, Just ch):acc) bn

-- this takes an fsm, a state, queue and seen & returns new queue
updateQueue :: Ord a => FSM ch a -> BNode a ch -> [BNode a ch] -> Set a -> [BNode a ch]
updateQueue fsm st queue seen = enqueue queue seen bNeighbours
  where
    neighbours = getNeighboursEv fsm $ node st
    bNeighbours = map (\(st', ch) -> BNode st' (Just (st, ch))) neighbours

enqueue :: Ord a => [BNode a ch] -> Set a -> [BNode a ch] -> [BNode a ch]
enqueue queue seen items = queue ++ filter (\item -> notMember (node item) seen) items

  --unlist (foldl' (flip (enqueueOne seen)) (mklist queue) items) -- queue ++ filter (\item -> not $ node item `elem` seen) items

  --foldr (enqueueOne seen) queue items

newtype DList a = DList ([a] -> [a])

mklist :: [a] -> DList a
mklist xs = DList (xs ++)

single :: a -> DList a
single x = DList (x:)

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
  | otherwise             = queue `mappend` (single item)

extractCalls :: Maybe [(a, Maybe ch)] -> Maybe [ch]
extractCalls list = mapMaybe snd <$> list













