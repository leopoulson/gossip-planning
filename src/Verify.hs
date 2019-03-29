module Verify where

import MakeGraphs

import qualified Model 
import Malvin.Gossip
import Malvin.Gossip.General


graph3 :: Graph
graph3 = exampleFromList [[0, 1], [1, 2], [2]]

precon = anyCall

t = eval (graph3, [(0, 1), (0, 2), (0, 1)]) (K 0 precon allExperts)

verifyCalls :: Model.EpistM -> [Model.Event] -> Form ->  Bool
verifyCalls ep calls f = verifyE (exampleFromList $ graphToGattinger ep) (callsToGattinger calls) f

verifyAllExperts :: Model.EpistM -> [Model.Event] -> Bool
verifyAllExperts ep evs = verifyCalls ep evs allExperts

verifyE :: Graph -> Sequence -> Form -> Bool
verifyE g sigma f = eval (g, sigma) f

-- of course, we need to fill the hole that null creates
-- we can use isSuccSequence, but it seems there's something he provides too 
verifyEmptyG :: Int -> Graph -> Bool
verifyEmptyG agents g = not $ any (isSuccSequence (g, [])) $ filter ((< (agents + 2)) . length) $ sequences precon (g, [])

verifyEmpty :: Model.EpistM -> Bool
verifyEmpty model = verifyEmptyG (length $ Model.agents model) . exampleFromList . graphToGattinger $ model

