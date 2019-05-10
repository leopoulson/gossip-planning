{-# LANGUAGE FlexibleInstances #-}

module MakeGraphs where

import Model

import Test.QuickCheck
import Data.Maybe (fromMaybe)
import Data.List (subsequences, sort)

-- These are appended with M just to avoid annoying namespace clashes
type CallM = (Int, Int)
type SequenceM = [CallM]
type Phonebook = [[Int]]

agentN :: Int
agentN = 4

instance Arbitrary (EpistM StateC GosProp) where
  arbitrary = standardEpistModel (getAgents agentN) <$> (sublistOf $ allNumbers agentN)

allNumbers :: Int -> [GosProp]
allNumbers n = [N i j | i <- (getAgents agentN), j <- getAgents agentN, i /= j]

-- This is just like generateModels, except we only have phone number knowledge;
-- no one knows each others secret already
generateModelsPhonebook :: [Agent] -> Precondition Call GosProp -> [(EpistM StateC GosProp, EventModel Call GosProp)]
generateModelsPhonebook ags pre = map (\ps -> (standardEpistModel ags ps, standardEventModel ags pre postUpdate)) $ validPhonebooks ags

generateModels :: [Agent] -> Precondition Call GosProp -> [(EpistM StateC GosProp, EventModel Call GosProp)]
generateModels ags pre = map (\ps -> (standardEpistModel ags ps, standardEventModel ags pre postUpdate)) $ validKnowledgeStates ags

validPhonebooks :: [Agent] -> [[GosProp]]
validPhonebooks ags = subsequences . allPhonebooks $ ags

idProps :: [Agent] -> [GosProp]
idProps ags = [S i i | i <- ags] ++ [N i i | i <- ags]

validKnowledgeStates :: [Agent] -> [[GosProp]]
validKnowledgeStates = filter isValid . subsequences . allKnowledge 

isValid :: [GosProp] -> Bool
isValid ps = all (\q -> possible q ps) ps

isPhonebook :: [GosProp] -> Bool
isPhonebook ps = all isN ps

possible :: GosProp -> [GosProp] -> Bool
possible (S i j) ps = (N i j) `elem` ps
possible (N _ _) _  = True

allPhonebooks :: [Agent] -> [GosProp]
allPhonebooks ags = [N i j | i <- ags, j <- ags, i /= j]

allKnowledge :: [Agent] -> [GosProp]
allKnowledge ags = [N i j | i <- ags, j <- ags] ++ [S i j | i <- ags, j <- ags]

-- We make the assumption that the actual state is actually the actual state
-- And that any information that we need is valuated at that statei
graphToGattinger :: EpistM StateC GosProp -> Phonebook
graphToGattinger (Mo _ ag val _ actual _) = map sort $ foldr updatePhonebook basePhonebook ns
  where
    props = concatMap (\act -> fromMaybe (error "No valuation result") $ lookup act val) actual
    ns = filter isN . map fromProp $ props
    intAgs = map (\(Ag n) -> n) ag
    basePhonebook = [[] | agn <- intAgs]



-- In order to update the phonebook with a call, we want to
-- Take the fact of one agent knowing another's phone number, and add in
-- at that position the other's phone number. 
updatePhonebook :: GosProp -> Phonebook -> Phonebook
updatePhonebook (N (Ag n1) (Ag n2)) phonebook = updateAt (phonebook !! n1 ++ [n2]) n1 phonebook

updateAt :: a -> Int -> [a] -> [a]
updateAt item n list = take n list ++ [item] ++ drop (n + 1) list

callsToGattinger :: [Call] -> SequenceM
callsToGattinger calls = map (changeCall) calls

changeCall :: Call -> CallM
changeCall (Call (Ag a) (Ag b)) = (a, b)

agN :: Agent -> Int
agN (Ag n) = n 

isN :: GosProp -> Bool
isN (N _ _) = True
isN _       = False

fromProp :: Form p -> p
fromProp (P p) = p
fromProp _     = error "Not prop"

isProp :: Form p -> Bool
isProp (P _) = True
isProp _     = False

getAgents :: Int -> [Agent]
getAgents n = Ag <$> [0 .. (n - 1)]
