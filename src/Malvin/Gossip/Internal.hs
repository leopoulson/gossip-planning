module Malvin.Gossip.Internal where

import Data.List (isPrefixOf)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set

at :: IntMap b -> Int -> b
at m k = x where (Just x) = IntMap.lookup k m

lfp :: Eq a => (a -> a) -> a -> a
lfp f x = if f x == x then x else lfp f (f x)

finiteIterate :: Int -> (a -> a) -> a -> a
finiteIterate 0 _ x = x
finiteIterate k f x = finiteIterate (k-1) f (f x)

splitWhere :: Eq a => a -> [a] -> [[a]]
splitWhere a str = case break (== a) str of
  (xs,[])    -> [xs]
  (xs,[_a])  -> [xs]
  (xs,_a:ys) -> xs : splitWhere a ys

setUnion :: Ord a => Set (Set a) -> Set a
setUnion = Set.unions . Set.toList

prefixElem :: Eq a => [a] -> [[a]] -> Bool
prefixElem xs = any (\ys -> xs `isPrefixOf` ys)
