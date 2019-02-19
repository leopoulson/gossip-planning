module Gossip where

import Model
import qualified Data.Map as M
import qualified Data.Set as S

type GGraph = ([Agent], KnowMap, KnowMap) 
type KnowMap = M.Map Agent (S.Set Agent)

--                  Number   Secret
type Call = (Agent, KnowMap, KnowMap)

example :: GGraph
example = ([a, b, c], M.fromList [(a, S.singleton b), (b, S.singleton c)], M.empty)

