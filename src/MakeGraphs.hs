module MakeGraphs where

import Model

import Data.List (subsequences)


generateModels :: [Agent] -> [(EpistM, EventModel)]
generateModels ags = map (\ps -> (standardEpistModel ags ps, standardEventModel ags anyCall postUpdate)) $ validKnowledgeStates ags

validKnowledgeStates :: [Agent] -> [[Prop]]
validKnowledgeStates = filter isValid . subsequences . allKnowledge 

isValid :: [Prop] -> Bool
isValid ps = all (\q -> possible q ps) ps

possible :: Prop -> [Prop] -> Bool
possible (S i j) ps = (N i j) `elem` ps
possible (N _ _) _  = True

allKnowledge :: [Agent] -> [Prop]
allKnowledge ags = [N i j | i <- ags, j <- ags, i /= j] ++ [S i j | i <- ags, j <- ags, i /= j]
