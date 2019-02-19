module Gossip where

import Model
import qualified Data.Map as M
import qualified Data.Set as S

--                                Number   Secret
newtype GGraph = Gossip ([Agent], KnowMap, KnowMap)
    deriving (Show, Eq)

type KnowMap = M.Map Agent (S.Set Agent)

type Call = (Agent, Agent)

gExample :: GGraph
gExample = Gossip ([a, b, c], M.fromList [(a, S.singleton b), (b, S.singleton c)], M.empty)

call :: GGraph -> Call -> GGraph
call g c = 