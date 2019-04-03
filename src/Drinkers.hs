{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Drinkers where

import ME
import Model
import FSM

import Prelude hiding (fst, snd)
import Data.Maybe (fromMaybe)
import Data.List (nub)

bools = [True, False]

type TP = (Bool, Bool, Bool) 

instance Prop TP where
  evalProp pr m w = (P pr) `elem` tval m w

data AmAn = DKA | DKB | DKC | AllC deriving (Eq, Show)

dkA, dkB, dkC, allC :: AmAn
dkA = DKA
dkB = DKB
dkC = DKC
allC = AllC

type StateB = State AmAn

-- instance EvState AmAn where
  -- lastEv (State (_, es)) = last es
  -- trimLast (State (w, es)) = State (w, init es)

makeState :: Int -> StateB
makeState n = State (n, [])

initBar :: EpistM StateB TP
initBar = Mo states [a, b, c] val rels [] alls-- [(True, True, True)]
  where
    states = [makeState n | n <- [0 .. 7]] --  [(b1, b2, b3) | b1 <- bools, b2 <- bools, b3 <- bools]
    rela = (a, [[ st | st <- states, pLookupEq fst (lookup st val) True], 
                [ st | st <- states, pLookupEq fst (lookup st val) False]])
    relb = (b, [[ st | st <- states, pLookupEq snd (lookup st val) True],
                [ st | st <- states, pLookupEq snd (lookup st val) False]])
    relc = (c, [[ st | st <- states, pLookupEq thd (lookup st val) True],
                [ st | st <- states, pLookupEq thd (lookup st val) False]])

    rels = [rela, relb, relc]
    val = zip [makeState n | n <- [0 .. 7]] [[P (b1, b2, b3)] | b1 <- bools, b2 <- bools, b3 <- bools]
    alls = [(b1, b2, b3) | b1 <- bools, b2 <- bools, b3 <- bools]

barEv :: EventModel AmAn TP
barEv = EvMo [dkA, dkB, dkC, allC] rels pre post
  where
    rels = [(ag, dks) | ag <- [a, b, c]]
    dks = [[dkA], [dkB], [dkC], [allC]]
    pre = preBar
    post = postBar

allBeer :: Form TP
allBeer = P (True, True, True)

preBar :: AmAn -> Form TP
-- preBar _ = Top
preBar (DKA) = (Or [P (True, True, True), P (True, True, False), P (True, False, True), P (True, False, False)])
preBar (DKB) = (Or [P (True, True, True), P (True, True, False), P (False, True, True), P (False, True, False)])
preBar (DKC) = (Or [P (True, True, True), P (True, False, True), P (False, True, True), P (False, False, True)])
preBar (AllC) = K c allBeer --(Or [P (True, True, False), P (True, True, True)])
-- preBar DKA = Or [Not (K a (Not allBeer)), Not (K a allBeer)]
-- preBar DKB = Or [Not (K b (Not allBeer)), Not (K b allBeer)]
-- preBar DKC = Or [Not (K c (Not allBeer)), Not (K c allBeer)]
-- preBar DKA = P (True, )

postBar :: (AmAn, TP) -> Form TP
-- postBar (DKA, (b1, b2, b3)) = P (True, b2, b3)
-- postBar (DKB, (b1, b2, b3)) = P (b1, True, b3)
-- postBar (DKC, (b1, b2, b3)) = P (b1, b2, True)
-- postBar (DKA, (True, b2, b3)) = P (True, b2, b3)
-- postBar (DKA, (False, _, _))  = Not Top
-- postBar (DKB, (b1, True, b3)) = P (b1, True, b3)
-- postBar (DKB, (_, False, _))  = Not Top
-- postBar (DKC, (b1, b2, True)) = P (b1, b2, True)
-- postBar (DKC, (_, _, False))  = Not Top
-- postBar (AllC, (True, True, True)) = Top
-- postBar (AllC, _) = Not Top
postBar (_, p) = P p


fst (x, _, _) = x
snd (_, y, _) = y
thd (_, _, z) = z

pLookupEq :: (TP -> Bool) -> Maybe [Form TP] -> Bool -> Bool
pLookupEq acc fm comp = (map (fmap acc) <$> fm) == Just [(P comp)]

mapValEp (Mo _ _ val rel _ _) = mapVal rel val

mapVal :: AgentRel StateB -> Valuation StateB TP -> AgentRel (Form TP)
mapVal ls val = [(a, nub $ map (concatMap (doVal val)) rel) | (a, rel) <- ls]
  where
    doVal :: Valuation StateB TP -> StateB -> [Form TP]
    doVal val st = fromMaybe (error "bad lookup") (lookup st val)

-- Tests ----------

dAuto = buildDAutomataNoF initBar barEv

t1 = transition dAuto (QInit, Left (State (0, [])))
