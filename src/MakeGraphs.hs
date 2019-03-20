module MakeGraphs where

import Model

import Data.List (subsequences)


generateModels :: [Agent] -> EpistM
generateModels ags = undefined

validKnowledgeStates :: [Agent] -> [[Prop]]
validKnowledgeStates ags = filter isValid . subsequences . allKnowledge $ ags

isValid :: [Prop] -> Bool
isValid ps = all (\q -> possible q ps) ps

possible :: Prop -> [Prop] -> Bool
possible (S i j) ps = (N i j) `elem` ps
possible (N _ _) _  = True

allKnowledge :: [Agent] -> [Prop]
allKnowledge ags = [N i j | i <- ags, j <- ags, i /= j] ++ [S i j | i <- ags, j <- ags, i /= j]
