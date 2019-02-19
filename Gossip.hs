module Gossip where

import Model
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

--                              Number  Secret
data GGraph = Gossip [Agent] KnowMap KnowMap
    deriving (Show, Eq)
-- data GGraph = Gossip [Agent] [Prop]
--     deriving (Show, Eq)

type KnowMap = M.Map Agent (S.Set Agent)
type Call = (Agent, Agent)

gExample :: GGraph
gExample = Gossip [a, b, c] (M.fromList [(a, S.fromList [a, b]), (b, S.fromList [b,c]), (c, S.empty)]) (M.fromList [(a, S.singleton a), (b, S.singleton b), (c, S.singleton c)])

possible :: Call -> GGraph -> Bool
possible (i, j) (Gossip _ n _) = S.member j (fromMaybe S.empty (M.lookup i n) )

performCall :: GGraph -> Call -> GGraph
performCall g@(Gossip ag n s) call@(i, j) 
    | not $ possible call g = error $ "impossible call " ++ show call
    | otherwise = Gossip ag n' s' where
        n' = M.adjust (S.union (n M.! i)) j $ M.adjust (S.union (n M.! j)) i n
        s' = M.adjust (S.union (s M.! i)) j $ M.adjust (S.union (s M.! j)) i s
    
performCalls :: [Call] -> GGraph -> GGraph
performCalls calls g = foldl performCall g calls











