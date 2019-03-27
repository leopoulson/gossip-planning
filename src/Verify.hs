module Verify where

import MakeGraphs

import qualified Model 
import Malvin.Gossip
import Malvin.Gossip.General


graph3 :: Graph
graph3 = exampleFromList [[0, 1], [1, 2], [2]]

t = eval (graph3, [(0, 1), (0, 2), (0, 1)]) (K 0 anyCall allExperts)

verifyCalls :: Model.EpistM -> [Model.Event] -> Form ->  Bool
verifyCalls ep calls f = verify (exampleFromList $ graphToGattinger ep) (callsToGattinger calls) f

verifyAllExperts :: Model.EpistM -> [Model.Event] -> Bool
verifyAllExperts ep evs = verifyCalls ep evs allExperts

verify :: Graph -> Sequence -> Form -> Bool
verify g sigma f = eval (g, sigma) f
