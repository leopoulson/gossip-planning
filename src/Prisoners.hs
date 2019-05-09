module Prisoners where

import Model
import FSM
import Powerset
import ME
import BFSM

import Data.Set as Set

data Pr = Qi Int | LightOn deriving (Eq, Ord, Show)

prisoners = 9

instance Prop Pr where
  evalProp pr m w = (P pr) `elem` tval m w
  allProps' _ = Set.fromList $ allPropsN prisoners

data EvI = Empty | On Int | Off Int deriving (Eq, Ord, Show)

allPropsN n = [LightOn] ++ [Qi n | n <- [0 .. (n - 1)]]




type StateI = State EvI

q1, q2, fin :: Form Pr
q1 = P (Qi 1)
q2 = P (Qi 2)
fin = K a (And [P (Qi n) | n <- [1 .. prisoners - 1]])

initM :: EpistM StateI Pr
initM = Mo 
    [State (0, [])]
    [a]
    [(State (0, []), [])]
    [(a, [[State (0, [])]])]
    [State (0, [])]
    (allProps' [a] :: Set Pr)

evmo :: EventModel EvI Pr
evmo = EvMo 
    ([Empty] ++ [On n | n <- [0 .. prisoners - 1]] ++ [Off n | n <- [0 .. prisoners - 1]])
    [(a, [[On 0], [Off 0], ([Empty] ++ [On n | n <- [1 .. prisoners - 1]] ++ [Off n | n <- [1 .. prisoners - 1]])])]
    prec
    postc


prec :: Precondition EvI Pr
prec Empty = Top
prec (On _) = P LightOn
prec (Off _) = Not (P LightOn)

postc :: Postcondition EvI Pr
postc (Empty, p) = P p
postc (On 0, LightOn) = Not Top
postc (On 0, p) = P p
postc (Off 0, p) = P p 
postc (On i, p) = P p
postc (Off i, LightOn) = Or [Not (P $ Qi i), P LightOn]
postc (Off i, Qi n) = if i == n then
        Or [Not $ P LightOn, P $ Qi i]
    else 
        P (Qi n)



prisonPSA :: FSM (Character EvI) (PState (QState Pr))
prisonPSA = createSolvingAutomata fin initM evmo tFilter









