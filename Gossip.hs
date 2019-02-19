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

-- gExample :: GGraph
-- gExample = Gossip [a, b, c] [N a b, N b c]

gExample :: GGraph
gExample = Gossip [a, b, c] (M.fromList [(a, S.singleton b), (b, S.singleton c)]) M.empty

possible :: Call -> GGraph -> Bool
possible (i, j) (Gossip _ n _) = S.member j (fromMaybe S.empty (M.lookup i n) )

-- call :: GGraph -> Call -> GGraph
-- call g c 
--     | possible c g  = 
--     | otherwise = error $ "impossible call " ++ show c




