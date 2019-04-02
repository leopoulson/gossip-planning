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

allBeer :: Form TP
allBeer = P (True, True, True)

initBar :: EpistM Int TP
initBar = Mo states [a, b, c] val rels [0]-- [(True, True, True)]
  where
    states = [0 .. 7] --  [(b1, b2, b3) | b1 <- bools, b2 <- bools, b3 <- bools]
    rela = (a, [[ st | st <- states, pLookupEq fst (lookup st val) True], 
                [ st | st <- states, pLookupEq fst (lookup st val) False]])
    relb = (b, [[ st | st <- states, pLookupEq snd (lookup st val) True],
                [ st | st <- states, pLookupEq snd (lookup st val) False]])
    relc = (c, [[ st | st <- states, pLookupEq thd (lookup st val) True],
                [ st | st <- states, pLookupEq thd (lookup st val) False]])

    rels = [rela, relb, relc]
    val = zip [0 ..] [[P (b1, b2, b3)] | b1 <- bools, b2 <- bools, b3 <- bools]

barEv :: EventModel AmAn TP
barEv = EvMo [dkA, dkB, dkC] rels pre post
  where
    rels = [(ag, dks) | ag <- [a, b, c]]
    dks = [[dkA], [dkB], [dkC]]
    pre = undefined
    -- post : (AmAn, Prop) -> Form prop
    
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

