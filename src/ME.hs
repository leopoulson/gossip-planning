{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module ME where

import Model
import FSM
import FST
import RS
import Data.Maybe
import Data.List (sort)
import qualified Data.Set as Set

import Debug.Trace

-- data Character = World State | Call Agent Agent deriving (Eq, Show)
-- type Alphabet = [Character]


type Character ev = Either (State ev) ev
type Alphabet ev = [Character ev]

type CallChar = Character Call

-- States in ME* are indexed just by the propositions that are true at them
-- So we can just let them *be* the propositions that are true at them
data QState p = Q (Set.Set p) | QInit deriving (Show, Ord)

instance (Eq p) => Eq (QState p) where
    QInit == QInit = True
    Q ps1 == Q ps2 = ps1 == ps2
    QInit == _     = False
    Q _   == _     = False

class (Ord st, Prop p) => EvalState st p where
  evalState :: Form p -> st -> Bool

instance (Ord p, Prop p) => EvalState (QState p) p where
  evalState f (Q ps) = models ps f
  evalState _ QInit  = error "Evaluation on QInit"

data ME p ch = ME 
    (FSM (Character ch) (QState p))
    [(Agent, FST (Character ch) (QState p))] 
    [(Agent, FSM (Character ch) (QState p))]

getAlphabet :: EpistM (State ev) p -> EventModel ev p -> Alphabet ev
getAlphabet ep evm = map Left (Model.states ep) ++ map Right (events evm)

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x : xs) = powerList xs ++ map (x:) (powerList xs)

getFStates :: Prop p => EpistM (State ev) p -> [QState p]
getFStates (Mo _ ags _ _ _ props)= fmap Q $ fmap Set.fromList $ powerList $ props

simpleAccept :: QState p -> Bool
simpleAccept (Q _)      = True
simpleAccept QInit      = False

getQStates :: Prop p => EpistM (State ev) p -> [QState p]
getQStates mo = getFStates mo ++ [QInit]

getvee :: EpistM st p -> Valuation st p
getvee (Mo _ _ valu _ _ _) = valu

isForm :: Form p -> Bool
isForm (P _) = True
isForm _ = False 

fromForm :: Prop p => Form p -> p
fromForm (P p) = p
fromForm f = error ("From form received " ++ show f)

getForms :: Prop p => [Form p] -> [p]
getForms = map fromForm . filter isForm

idProps :: [Agent] -> [GosProp]
idProps ags = [S i j | i <- ags, j <- ags, i == j] ++ [N i j | i <- ags, j <- ags, i == j]

meTrans :: (Prop p, Eq ev) => EpistM (State ev) p -> EventModel ev p -> Transition (QState p) (Character ev)
meTrans (Mo _ _ v _ _ _)    _                (QInit, Left state)   = Just $ Q . Set.fromList . getForms $ fromMaybe undefined (lookup state v)
meTrans _                 _                (QInit, Right _)      = Nothing
meTrans _                 _                (Q _  , Left _)       = Nothing
meTrans (Mo _ ags _ _ _ props)  evm              (Q ps , Right ev)
    | not $ psID `listModels` pre evm ev                             = Nothing
    -- This is quite bad. Doing Set.fromList here is very costly. Must find another way to do this. 
    | otherwise                                                  = Just $  Q . Set.fromList $ [p | p <- props, psID `listModels` post evm (ev, p)]
    where
        psID = Set.toList ps -- ++ idProps ags

evalQState :: Prop p => Form p -> QState p -> Bool
evalQState form (Q ps) = models ps form
evalQState _ QInit          = error "Cannot evaluate QInit"

models :: Prop p => Set.Set p -> Form p -> Bool
models _  Top         = True
models ps (Not form)  = not $ models ps form
models ps (P form)    = Set.member form ps
models ps (Or forms)  = any (models ps) forms
models ps (And forms) = all (models ps) forms
models _  (K _ _)     = error "Cannot evaluate K φ on a set of props"


listModels :: Prop p => [p] -> Form p -> Bool
listModels _  Top         = True
listModels ps (Not form)  = not $ listModels ps form
listModels ps (P form)    = form `elem` ps
listModels ps (Or forms)  = any (listModels ps) forms
listModels ps (And forms) = all (listModels ps) forms
listModels _  (K _ _)     = error "Cannot evaluate K φ on a set of props"



buildTransducers :: Eq ev => EpistM (State ev) p -> EventModel ev p -> [(Agent, FST (Character ev) (QState p))]
buildTransducers ep ev = [(agent, buildTransducer agent ep ev) | agent <- agents ep]

buildSSTransducer :: Eq ev => Agent -> EpistM (State ev) p -> EventModel ev p -> SSFST (Character ev)
buildSSTransducer ag ep evm = SSFST (getAlphabet ep evm) trans 
  where
    --trans :: SSTransition Character
    trans (Left w)  =  [Left w'   | w'  <- relatedWorldsAgent (eprel ep) ag w]
    trans (Right ev) = [Right ev' | ev' <- relatedWorldsAgent (evrel evm) ag ev]

buildTransducer :: Eq ev => Agent -> EpistM (State ev) p -> EventModel ev p -> FST (Character ev) (QState p)
buildTransducer ag ep evm = FST (getAlphabet ep evm) [QInit] trans [QInit] acc
  where
    --trans :: BiTransition QState Character
    trans (QInit, Left w) = [(Left w', QInit) | w' <- relatedWorldsAgent (eprel ep) ag w]
    trans (QInit, Right ev) = [(Right ev', QInit) | ev' <- relatedWorldsAgent (evrel evm) ag ev]
    trans (Q _, _) = error "No transition for states other than QInit"
    --acc :: QState -> Bool
    acc QInit = True
    acc _ = False

identityTransducer :: Eq ev => FSM (Character ev) st -> FST (Character ev) st
identityTransducer (FSM alpha sts trans int accept) = 
    FST alpha sts trans' int accept where
        -- trans' :: BiTransition st Character
        trans' (st, ch) = case trans (st, ch) of --[(ch, trans (st, ch))]
            Just q  -> [(ch, q)]
            Nothing -> []

buildComposedSS :: Eq ev => Agent -> EpistM (State ev) p -> EventModel ev p -> FSM (Character ev) st -> FST (Character ev) st
buildComposedSS ag ep evm fsm = buildSSTransducer ag ep evm `composeSS` identityTransducer fsm

-- This and the below functions are probably not used at all, can delete

-- pAutomata :: FSM Character QState -> Prop -> FSM Character QState
-- pAutomata (FSM alpha sts trans int accept) pr = 
    -- FSM alpha sts trans int accepting' where
        -- accepting' = pUpdate' pr accept

-- pUpdate :: p -> (QState p, Bool) -> (QState p, Bool)
-- pUpdate p (Q ps, _)  = (Q ps, p `elem` ps)
-- pUpdate _ qsb = qsb

-- pUpdate' :: p -> (QState p -> Bool) -> (QState p -> Bool)
-- pUpdate' _ f QInit  = f QInit
-- pUpdate' p f (Q ps) 
  -- | p `elem` ps = True
  -- | otherwise   = f (Q ps)

setSuccessfulFormula :: EvalState st p => Form p -> FSM ch st -> FSM ch st
setSuccessfulFormula f = updateAcccepting (evalState f) 

-- We know now that we can make this better.
-- * Set initial states from the event model?
buildDAutomataNoF :: (Eq ev, Prop p) => EpistM (State ev) p -> EventModel ev p -> FSM (Character ev) (QState p)
buildDAutomataNoF ep ev = FSM 
    (getAlphabet ep ev)
    (getQStates ep)
    (meTrans ep ev)
    [QInit]
    simpleAccept

buildDAutomata :: (Eq ev, Prop p) => Form p -> EpistM (State ev) p -> EventModel ev p -> FSM (Character ev) (QState p)
buildDAutomata f ep ev = setStatesReachableInit $ buildDAutomataCore f ep ev

-- We set states to be undefined, as they're set in BDABetter
-- This is because they need to be done after we've set initial state
buildDAutomataCore :: (Eq ev, Prop p) => Form p -> EpistM (State ev) p -> EventModel ev p -> FSM (Character ev) (QState p)
buildDAutomataCore f ep ev = FSM
    (getAlphabet ep ev)
    undefined 
    (meTrans ep ev) 
    (getInit ep)
    (evalState f)

getInit :: Prop p => Eq st => EpistM st p -> [QState p]
getInit (Mo _ _ val _ actual _) = Q <$> map (\st -> Set.fromList . map fromForm . fromMaybe [] $ lookup st val) actual

buildMEStar :: (Eq ev, Prop p) => EpistM (State ev) p -> EventModel ev p -> RegularStructure (Character ev) (QState p)
buildMEStar ep ev = RegularStructure 
    dAuto 
    [(ag, buildComposedSS ag ep ev dAuto) | ag <- agents ep]
    -- (pAutomata dAuto)
  where 
    dAuto = buildDAutomataNoF ep ev

setEq :: Ord a => Set.Set a -> Set.Set a -> Bool
setEq m n = (m `Set.isSubsetOf` n) && (n `Set.isSubsetOf` m)


