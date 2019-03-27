module Malvin.Verify where

import Malvin.Gossip
import Malvin.Gossip.General

-- This file is for verifying output from the call string generation program.

verify :: Graph -> Sequence -> Form -> Bool
verify g sigma f = eval (g, sigma) f 

graph3 = exampleFromList [[0, 1], [1, 2], [2]]

t = eval (graph3, [(0, 1), (0, 2), (0, 1)]) (K 0 anyCall allExperts)
