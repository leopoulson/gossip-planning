module ME where

import Model
import FSM
import FST
import RS
import Data.Maybe
import Data.List (sort)

-- data Character = World State | Call Agent Agent deriving (Eq, Show)
-- type Alphabet = [Character]

type Character = Either State Event
type Alphabet = [Character]

-- States in ME* are indexed just by the propositions that are true at them
-- So we can just let them *be* the propositions that are true at them
data QState = Q [Prop] | QInit deriving (Show)

instance Eq QState where
    QInit == QInit = True
    Q ps1 == Q ps2 = sort ps1 == sort ps2
    QInit == _     = False   
    Q _   == _     = False 

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
fromForm f = error ("From form received " ++ show f)

getForms :: [Form] -> [Prop]
getForms = map fromForm . filter isForm

idProps :: [Agent] -> [Prop]
idProps ags = [S i j | i <- ags, j <- ags, i == j] ++ [N i j | i <- ags, j <- ags, i == j]

meTrans :: EpistM -> EventModel -> Transition QState Character
meTrans (Mo _ _ v _ _)    _                (QInit, Left state)   = Q . getForms $ fromMaybe undefined (lookup state v)
meTrans _                 _                (QInit, Right _)      = undefined -- Reject input
meTrans _                 _                (Q _  , Left _)       = undefined -- Reject input
meTrans (Mo _ ags _ _ _) evm (Q ps , Right ev) 
    | not $ psID `models` pre evm ev              = error "Doesn't satisfy precondition"
    | otherwise                                                  = Q [p | p <- produceAllProps ags, psID `models` post evm (ev, p)]         
    where
        psID = ps ++ idProps ags        

models :: [Prop] -> Form -> Bool
models _  Top         = True
models ps (Not form)  = not $ models ps form
models ps (P form)    = form `elem` ps
models ps (Or forms)  = any (models ps) forms
models ps (And forms) = all (models ps) forms
models _  (K _ _)     = error "How to model K?"

buildTransducers :: EpistM -> EventModel -> [(Agent, FST Character QState)]
buildTransducers ep ev = [(agent, buildTransducer agent ep ev) | agent <- agents ep]

buildTransducer :: Agent -> EpistM -> EventModel -> FST Character QState
buildTransducer ag ep evm = FST (getAlphabet ep evm) [QInit] trans [QInit] acc
  where
    trans :: BiTransition QState Character
    trans (QInit, Left w) = [(Left w', QInit) | w' <- relatedWorldsAgent (eprel ep) ag w]
    trans (QInit, Right ev) = [(Right ev', QInit) | ev' <- relatedWorldsAgent (evrel evm) ag ev]
    trans (Q _, _) = error "No transition for states other than QInit"
    acc :: QState -> Bool
    acc QInit = True
    acc _ = False

identityTransducer :: FSM Character QState -> FST Character QState
identityTransducer (FSM alpha states trans initial accepting) = 
    FST alpha states trans' initial accepting where
        trans' :: BiTransition QState Character
        trans' (st, ch) = [(ch, trans (st, ch))]
        -- trans' (QInit, Left state) = [(Left state, trans (QInit, Left state))]
        -- trans' (Q ps, Right ev) = [(Right ev, trans (Q ps, Right ev))]
        -- trans' _ = undefined

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








