module Profile where

import Model
import Powerset
import ME
import BFSM
import MakeGraphs
import Verify

import Malvin.Gossip.Strengthening
import qualified Malvin.Gossip.General as Malvin
import qualified Malvin.Gossip as Malvin.Gossip

import Test.QuickCheck
import Data.Maybe (fromMaybe)
import Data.List (subsequences, sort)

import Control.Monad (replicateM)

type GossipModel = EpistM StateC GosProp

prec :: Precondition Call GosProp
prec = lns

successfulFormula :: [Agent] -> Form GosProp
successfulFormula = allExpertsAg --abKnowAllExperts

malvinSuccessfulFormula :: Malvin.Form
malvinSuccessfulFormula = Malvin.allExperts

genGossip :: IO (EpistM StateC GosProp)
genGossip = generate (arbitrary :: Gen (EpistM StateC GosProp))

-- performN :: Int -> IO [(Maybe [CallChar], (Int, Int))]
performN n = replicateM n $ performBoth <$> genGossip

-- We can fmap this into genGossip to perform it!
perform :: GossipModel -> Maybe [CallChar]
perform model = extractCalls . doBFS . getPSA $ model

performMalvin :: GossipModel -> (Int, Int)
performMalvin model = statistics' Malvin.lns (ex, []) malvinSuccessfulFormula
  where
    ex = Malvin.Gossip.exampleFromList $ graphToGattinger model

-- performBoth :: GossipModel -> (Maybe [CallChar], (Int, Int))
-- performBoth model = putStrLn $ show $ (perform model, performMalvin model)
performBoth model = (perform model, performMalvin model)

getPSA :: EpistM StateC GosProp -> PSA Call GosProp
getPSA mo = createSolvingAutomata (successfulFormula $ agents mo) mo (standardEventModel (agents mo) prec postUpdate) knowFilter
