module ME where

import Model
import FSM
import FST
import RS
import Data.Maybe

-- data Character = World State | Call Agent Agent deriving (Eq, Show)
-- type Alphabet = [Character]

type Character = Either State Event
type Alphabet = [Character]

-- States in ME* are indexed just by the propositions that are true at them
-- So we can just let them *be* the propositions that are true at them
data QState = Q [Prop] | QInit deriving (Show, Eq)

data ME = ME 
    (FSM Character QState)
    [(Agent, FST Character QState)] 
    [(Agent, FSM Character QState)]

getAlphabet :: EpistM -> EventModel -> Alphabet
getAlphabet ep evm = map Left (Model.states ep) ++ map Right (events evm)

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x : xs) = powerList xs ++ map (x:) (powerList xs)

getFStates :: EpistM -> [QState]
getFStates (Mo _ ags _ _ _ ) = fmap Q $ powerList $ produceAllProps ags

simpleAccept :: QState -> Bool
simpleAccept (Q _)      = True
simpleAccept QInit      = False

getQStates :: EpistM -> [QState]
getQStates mo = getFStates mo ++ [QInit]

getvee :: EpistM -> Valuation
getvee (Mo _ _ valu _ _) = valu

isForm :: Form -> Bool
isForm (P _) = True
isForm _ = False 

fromForm :: Form -> Prop
fromForm (P p) = p
fromForm _ = undefined --eek what happens here 

getForms :: [Form] -> [Prop]
getForms = map fromForm . filter isForm

meTrans :: EpistM -> EventModel -> Transition QState Character
meTrans (Mo _ _ v _ _)    _                (QInit, Left state)   = Q . getForms $ fromMaybe undefined (lookup state v)
meTrans _                 _                (QInit, Right _)      = undefined -- Reject input
meTrans _                 _                (Q _  , Left _)       = undefined -- Reject input
meTrans (Mo _ ags _ _ _) evm (Q ps , Right ev) 
    | not $ ps `models` pre evm ev                               = undefined -- Reject input
    | otherwise                                                  = Q [p | p <- produceAllProps ags, ps `models` post evm (ev, p)]

models :: [Prop] -> Form -> Bool
models ps f = fromForm f `elem` ps

buildTransducers :: EpistM -> EventModel -> [(Agent, FST Character QState)]
buildTransducers ep@(Mo _ agents _ _ _) ev = [(agent, buildTransducer agent ep ev) | agent <- agents]

buildTransducer :: Agent -> EpistM -> EventModel -> FST Character QState
buildTransducer ag ep ev = FST (getAlphabet ep ev) [QInit] trans [QInit] acc
  where
    trans :: BiTransition QState Character
    trans (QInit, Left w) = [(Left w', QInit) | w' <- relatedWorldsAgent (eprel ep) ag w]
    trans (QInit, Right e) = [(Right e', QInit) | e' <- relatedWorldsAgent (evrel ev) ag e]
    trans _ = undefined
    acc :: QState -> Bool
    acc QInit = True
    acc _ = False

identityTransducer :: FSM Character QState -> FST Character QState
identityTransducer (FSM alpha states trans initial accepting) = 
    FST alpha states trans' initial accepting where
        trans' :: BiTransition QState Character
        trans' (QInit, Left state) = [(Left state, trans (QInit, Left state))]
        trans' (Q ps, Right ev) = [(Right ev, trans (Q ps, Right ev))]
        trans' _ = undefined

buildComposedTransducers :: Agent -> EpistM -> EventModel -> FSM Character QState -> FST Character ((QState, QState), QState)
buildComposedTransducers ag ep ev fsm = idt `composeFST` buildTransducer ag ep ev `composeFST` idt 
  where
    idt = identityTransducer fsm

pAutomata :: FSM Character QState -> Prop -> FSM Character QState
pAutomata (FSM alpha states trans initial accepting) pr = 
    FSM alpha states trans initial accepting' where
        accepting' = pUpdate' pr accepting

pUpdate :: Prop -> (QState, Bool) -> (QState, Bool)
pUpdate p (Q ps, _)  = (Q ps, p `elem` ps)
pUpdate _ qsb = qsb

pUpdate' :: Prop -> (QState -> Bool) -> (QState -> Bool)
pUpdate' _ f QInit  = f QInit
pUpdate' p f (Q ps) 
  | p `elem` ps = True
  | otherwise   = f (Q ps)

buildDAutomata :: EpistM -> EventModel -> FSM Character QState
buildDAutomata ep ev = FSM 
    (getAlphabet ep ev)
    (getQStates ep)
    (meTrans ep ev)
    [QInit]
    simpleAccept    

buildMEStar :: EpistM -> EventModel -> RegularStructure Character QState
buildMEStar ep ev = RegularStructure 
    dAuto 
    [(ag, buildComposedTransducers ag ep ev dAuto) | ag <- agents ep]
    (pAutomata dAuto)
  where 
    dAuto = buildDAutomata ep ev








