module MakeGraphs where

import Model

import Data.Maybe (fromMaybe)
import Data.List (subsequences, sort)


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

-- We make the assumption that the actual state is actually the actual state
-- And that any information that we need is valuated at that statei
graphToGattinger :: EpistM -> [[Int]]
graphToGattinger (Mo _ ag val _ actual) = map sort $ foldr updatePhonebook basePhonebook ns
  where
    props = concatMap (\act -> fromMaybe (error "No valuation result") $ lookup act val) actual
    ns = filter isN . map fromProp $ props
    intAgs = map (\(Ag n) -> n) ag
    basePhonebook = [[agn] | agn <- intAgs]

-- In order to update the phonebook with a call, we want to
-- Take the fact of one agent knowing another's phone number, and add in
-- at that position the other's phone number. 
updatePhonebook :: Prop -> [[Int]] -> [[Int]]
updatePhonebook (N (Ag n1) (Ag n2)) phonebook = updateAt (phonebook !! n1 ++ [n2]) n1 phonebook

updateAt :: a -> Int -> [a] -> [a]
updateAt item n list = take n list ++ [item] ++ drop (n + 1) list

agN :: Agent -> Int
agN (Ag n) = n 

isN :: Prop -> Bool
isN (N _ _) = True
isN _       = False

fromProp :: Form -> Prop
fromProp (P p) = p
fromProp _     = error "Not prop"

isProp :: Form -> Bool
isProp (P _) = True
isProp _     = False
