module Drinkers where

import Prelude hiding (fst, snd)
import Model

bools = [True, False]

type TP = (Bool, Bool, Bool)

data AmAn = DKA | DKB | DKC deriving (Eq)

dkA, dkB, dkC :: AmAn
dkA = DKA
dkB = DKB
dkC = DKC

type StateB = State AmAn

-- instance EvState AmAn where
  -- lastEv (State (_, es)) = last es
  -- trimLast (State (w, es)) = State (w, init es)

makeState :: Int -> StateB
makeState n = State (n, [])

allBeer :: Form TP
allBeer = P (True, True, True)

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
barEv = EvMo [dkA, dkB, dkC] rels pre post
  where
    rels = [(ag, dks) | ag <- [a, b, c]]
    dks = [[dkA], [dkB], [dkC]]
    pre = preBar
    post = postBar

preBar :: AmAn -> Form TP
preBar (DKA) = (Or [P (True, True, True), P (True, True, False), P (True, False, True), P (True, False, False)])
preBar (DKB) = (Or [P (True, True, True), P (True, True, False), P (False, True, True), P (False, True, False)])
preBar (DKC) = (Or [P (True, True, True), P (True, False, True), P (False, True, True), P (False, False, True)])

postBar :: (AmAn, TP) -> Form TP
postBar (DKA, (b1, b2, b3)) = P (True, b2, b3)
postBar (DKB, (b1, b2, b3)) = P (b1, True, b3)
postBar (DKC, (b1, b2, b3)) = P (b1, b2, True)

fst (x, _, _) = x
snd (_, y, _) = y
thd (_, _, z) = z

pLookupEq :: (TP -> Bool) -> Maybe [Form TP] -> Bool -> Bool
pLookupEq acc fm comp = (map (fmap acc) <$> fm) == Just [(P comp)]

