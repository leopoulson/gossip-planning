module BFSM where

{-# LANGUAGE BangPatterns #-}


import FSM

import qualified Data.Sequence as Seq
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.Set (Set, empty, insert, notMember, member, size)

import Debug.Trace

-- can probably use a lens here !!!!!! lol
data BNode a ch = BNode {
    node   :: !a,       -- The actual node that we're looking at
    parent :: !(Maybe (BNode a ch, ch))  -- We might not have a predecessor yet
} deriving (Eq)

instance (Show a, Show ch) => Show (BNode a ch) where
  show (BNode a ch) = show a ++ " Parent " ++ show ch

bNode :: Show a => a -> BNode a ch
bNode a = BNode a Nothing

bNodePred :: a -> BNode a ch -> ch -> BNode a ch
bNodePred n p ch = BNode n (Just (p, ch))

doBFS :: (Ord a, Show a, Show ch) => FSM ch a -> Maybe [(a, Maybe ch)]
doBFS fsm = bfs fsm (map bNode $ initial fsm) empty

doBFS' :: (Ord a, Show a) => FSM ch a -> Maybe [(a, Maybe ch)]
doBFS' fsm = bfs' fsm (Seq.fromList $ map bNode $ initial fsm) empty

bfs :: (Ord a, Show a, Show ch) => FSM ch a -> [BNode a ch] -> Set a -> Maybe [(a, Maybe ch)]
bfs fsm queue seen = trace ("New call") $
  -- trace ("Seen length: " ++ (show $ size seen)) $
  case queue of
    []     -> trace ("Empty queue") $ Nothing    -- If the queue is empty, we stop and that is that
    (q:qs) -> case accepting fsm $ node q of
            True  -> Just $ rebuildPath q                                        -- Here construct the path
            False -> bfs fsm (updateQueue fsm q qs seen) (insert (node q) seen)  -- Here we want to recurse 
      -- if (node q) `notMember` seen then
          --trace ("Node: " ++ show (node q) ++ "\n") $
          -- case accepting fsm $ node q of
            -- True  -> Just $ rebuildPath q                                     -- Here construct the path
            -- False -> bfs fsm (updateQueue fsm q qs seen) (insert (node q) seen)   -- Here we want to recurse
       -- else  bfs fsm qs seen

-- this takes an fsm, a state, queue and seen & returns new queue
updateQueue :: (Ord a, Show ch) => FSM ch a -> BNode a ch -> [BNode a ch] -> Set a -> [BNode a ch]
updateQueue fsm st queue seen = enqueue queue seen bNeighbours
  where
    neighbours = getNeighboursEv fsm $ node st
    bNeighbours = map (\(st', ch) -> trace ("Neighbour " ++ show ch) $ BNode st' (Just (st, ch))) neighbours

enqueue :: Ord a => [BNode a ch] -> Set a -> [BNode a ch] -> [BNode a ch]
enqueue queue seen items = unlist (foldl' (flip (enqueueOne seen)) (mklist queue) items) -- queue ++ filter (\item -> notMember (node item) seen) items

bfs'' :: Ord a => FSM ch a -> Seq.Seq (BNode a ch) -> Set a -> Maybe [(a, Maybe ch)]
bfs'' fsm queue seen = case Seq.viewl queue of
    Seq.EmptyL  -> Nothing
    q Seq.:< qs -> case accepting fsm $ node q of
        True  -> Just $ rebuildPath q                                     -- Here construct the path 
        False -> bfs' fsm (updateQueue' fsm q qs seen) (insert (node q) seen)   -- Here we want to recurse

bfs' :: Ord a => FSM ch a -> Seq.Seq (BNode a ch) -> Set a -> Maybe [(a, Maybe ch)]
bfs' fsm Seq.Empty seen = Nothing
bfs' fsm (q Seq.:<| qs) seen = case accepting fsm $ node q of
        True  -> Just $ rebuildPath q                                     -- Here construct the path 
        False -> bfs' fsm (updateQueue' fsm q qs seen) (insert (node q) seen)   -- Here we want to recurse



updateQueue' :: Ord a => FSM ch a -> BNode a ch -> Seq.Seq (BNode a ch) -> Set a -> Seq.Seq (BNode a ch)
updateQueue' fsm st queue seen = enqueue' queue seen bNeighbours
  where
    neighbours = getNeighboursEv fsm $ node st
    bNeighbours = map (\(st', ch) -> BNode st' (Just (st, ch))) neighbours

enqueue' :: Ord a => Seq.Seq (BNode a ch) -> Set a -> [BNode a ch] -> Seq.Seq (BNode a ch)
-- enqueue' queue seen items = foldl' (flip $ enqueueOne' seen) queue items
enqueue' queue seen items = queue Seq.>< (Seq.fromList (filter (\item -> not$ member (node item) seen) items))

enqueueOne' :: Ord a => Set a -> BNode a ch -> Seq.Seq (BNode a ch) -> Seq.Seq (BNode a ch)
enqueueOne' seen item queue
  | notMember (node item) seen = queue Seq.|> item
  | otherwise                  = queue

  --queue ++ filter (\item -> notMember (node item) seen) items
  --unlist (foldl' (flip (enqueueOne seen)) (mklist queue) items)

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
enqueueOne :: Ord a => Set a -> BNode a ch -> DList (BNode a ch) -> DList (BNode a ch)
enqueueOne seen item queue
  | notMember (node item) seen = queue `mappend` (single item)
  | otherwise                  = queue

extractCalls :: Maybe [(a, Maybe ch)] -> Maybe [ch]
extractCalls list = mapMaybe snd <$> list

rebuildPath :: BNode a ch -> [(a, Maybe ch)]
rebuildPath = trace ("Rebuilding\n") $ go []
  where
    go acc (BNode x Nothing)         = (x, Nothing):acc
    go acc (BNode x (Just (bn, ch))) = go ((x, Just ch):acc) bn













