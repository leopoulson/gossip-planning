module Profile where

import Model
import Powerset
import ME
import FSM (removeLoopsFSM)
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
successfulFormula ags = K c $ K b $ K a (allExpertsAg ags)
    --allKnowAllExperts ags --K b $ K a (allExpertsAg ags) --abKnowAllExperts

allKnowAllExperts ags = And $ [K ag (allExpertsAg ags) | ag <- ags]

malvinSuccessfulFormula :: Malvin.Form
malvinSuccessfulFormula = Malvin.K 2 Malvin.lns $ Malvin.K 1 Malvin.lns $ Malvin.K 0 Malvin.lns Malvin.allExperts
    --Profile.allKnowExperts--Malvin.K 1 Malvin.lns $ Malvin.K 0 Malvin.lns Malvin.allExperts

allKnowExperts = Malvin.Conj [Malvin.K 0 Malvin.lns Malvin.allExperts, 
                              Malvin.K 1 Malvin.lns Malvin.allExperts, 
                              Malvin.K 2 Malvin.lns Malvin.allExperts, 
                              Malvin.K 3 Malvin.lns Malvin.allExperts]


genGossip :: IO (EpistM StateC GosProp)
genGossip = generate (arbitrary :: Gen (EpistM StateC GosProp))

-- performN :: Int -> IO [(Maybe [CallChar], (Int, Int))]
performNBoth n = replicateM n $ performBoth <$> genGossip
performNMine n = replicateM n $ perform <$> genGossip
performNMalv n = replicateM n $ performMalvin <$> genGossip

-- We can fmap this into genGossip to perform it!
perform :: GossipModel -> Maybe [CallChar]
perform model = extractCalls . doBFS . removeLoopsFSM . getPSA $ model

-- performMalvin :: GossipModel -> Maybe [(Int, Int)]
-- performMalvin :: GossipModel -> (Int, Int)
-- performMalvin model = Malvin.firstSucc Malvin.lns (ex, []) malvinSuccessfulFormula
performMalvin model = statistics' Malvin.lns (ex, []) malvinSuccessfulFormula
  where
    ex = Malvin.Gossip.exampleFromList $ graphToGattinger model

-- performBoth :: GossipModel -> (Maybe [CallChar], (Int, Int))
-- performBoth model = putStrLn $ show $ (perform model, performMalvin model)
performBoth model = (perform model, performMalvin model)

getPSA :: EpistM StateC GosProp -> PSA Call GosProp
getPSA mo = createSolvingAutomata (successfulFormula $ agents mo) mo (standardEventModel (agents mo) prec postUpdate) knowFilter
