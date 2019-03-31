module ME where

import Model
import FSM
import FST
import RS
import Data.Maybe
import Data.List (sort)
import qualified Data.Set as Set

-- data Character = World State | Call Agent Agent deriving (Eq, Show)
-- type Alphabet = [Character]

type Character = Either State Event
type Alphabet = [Character]

-- This gives us state evaluation into the type
class Ord st => EvalState st where 
  evalState :: Form -> st -> Bool

-- States in ME* are indexed just by the propositions that are true at them
-- So we can just let them *be* the propositions that are true at them
data QState = Q (Set.Set Prop) | QInit deriving (Show, Ord)

instance Eq QState where
    QInit == QInit = True
    Q ps1 == Q ps2 = ps1 == ps2
    QInit == _     = False   
    Q _   == _     = False 

instance EvalState QState where
    evalState f (Q ps)  = models ps f
    evalState _ QInit   = error "Evaluation on QInit"

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
getFStates (Mo _ ags _ _ _ ) = fmap Q $ fmap Set.fromList $ powerList $ produceAllProps ags

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
meTrans (Mo _ _ v _ _)    _                (QInit, Left state)   = Just $  Q . Set.fromList . getForms $ fromMaybe undefined (lookup state v)
meTrans _                 _                (QInit, Right _)      = Nothing
meTrans _                 _                (Q _  , Left _)       = Nothing
meTrans (Mo _ ags _ _ _)  evm              (Q ps , Right ev)
    | not $ psID `listModels` pre evm ev                             = Nothing
    -- This is quite bad. Doing Set.fromList here is very costly. Must find another way to do this. 
    | otherwise                                                  = Just $  Q . Set.fromList $ [p | p <- produceAllProps ags, psID `listModels` post evm (ev, p)]
    where
        psID = Set.toList ps ++ idProps ags

evalQState :: Form -> QState -> Bool
evalQState form (Q ps) = models ps form
evalQState _ QInit          = error "Cannot evaluate QInit"

models :: Set.Set Prop -> Form -> Bool
models _  Top         = True
models ps (Not form)  = not $ models ps form
models ps (P form)    = Set.member form ps
models ps (Or forms)  = any (models ps) forms
models ps (And forms) = all (models ps) forms
models _  (K _ _)     = error "Cannot evaluate K φ on a set of props"


listModels :: [Prop] -> Form -> Bool
listModels _  Top         = True
listModels ps (Not form)  = not $ listModels ps form
listModels ps (P form)    = form `elem` ps
listModels ps (Or forms)  = any (listModels ps) forms
listModels ps (And forms) = all (listModels ps) forms
listModels _  (K _ _)     = error "Cannot evaluate K φ on a set of props"



buildTransducers :: EpistM -> EventModel -> [(Agent, FST Character QState)]
buildTransducers ep ev = [(agent, buildTransducer agent ep ev) | agent <- agents ep]

buildSSTransducer :: Agent -> EpistM -> EventModel -> SSFST Character
buildSSTransducer ag ep evm = SSFST (getAlphabet ep evm) trans 
  where
    trans :: SSTransition Character
    trans (Left w)  =  [Left w'   | w'  <- relatedWorldsAgent (eprel ep) ag w]
    trans (Right ev) = [Right ev' | ev' <- relatedWorldsAgent (evrel evm) ag ev]

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

identityTransducer :: FSM Character st -> FST Character st
identityTransducer (FSM alpha sts trans int accept) = 
    FST alpha sts trans' int accept where
        -- trans' :: BiTransition st Character
        trans' (st, ch) = case trans (st, ch) of --[(ch, trans (st, ch))]
            Just q  -> [(ch, q)]
            Nothing -> []

buildComposedTransducers :: Agent -> EpistM -> EventModel -> FSM Character QState -> FST Character ((QState, QState), QState)
buildComposedTransducers ag ep ev fsm = idt `composeFST` buildTransducer ag ep ev `composeFST` idt 
  where
    idt = identityTransducer fsm

buildComposedSS :: Agent -> EpistM -> EventModel -> FSM Character st -> FST Character st
buildComposedSS ag ep evm fsm = buildSSTransducer ag ep evm `composeSS` identityTransducer fsm

pAutomata :: FSM Character QState -> Prop -> FSM Character QState
pAutomata (FSM alpha sts trans int accept) pr = 
    FSM alpha sts trans int accepting' where
        accepting' = pUpdate' pr accept

pUpdate :: Prop -> (QState, Bool) -> (QState, Bool)
pUpdate p (Q ps, _)  = (Q ps, p `elem` ps)
pUpdate _ qsb = qsb

pUpdate' :: Prop -> (QState -> Bool) -> (QState -> Bool)
pUpdate' _ f QInit  = f QInit
pUpdate' p f (Q ps) 
  | p `elem` ps = True
  | otherwise   = f (Q ps)

setSuccessfulFormula :: EvalState st => Form -> FSM ch st -> FSM ch st
setSuccessfulFormula f = updateAcccepting (evalState f) 

-- We know now that we can make this better.
-- * Set initial states from the event model?
buildDAutomataNoF :: EpistM -> EventModel -> FSM Character QState
buildDAutomataNoF ep ev = FSM 
    (getAlphabet ep ev)
    (getQStates ep)
    (meTrans ep ev)
    [QInit]
    simpleAccept

buildDAutomata :: Form -> EpistM -> EventModel -> FSM Character QState
buildDAutomata f ep ev = setStatesReachableInit $ buildDAutomataCore f ep ev

-- We set states to be undefined, as they're set in BDABetter
-- This is because they need to be done after we've set initial state
buildDAutomataCore :: Form -> EpistM -> EventModel -> FSM Character QState
buildDAutomataCore f ep ev = FSM
    (getAlphabet ep ev)
    undefined 
    (meTrans ep ev) 
    (getInit ep)
    (evalState f)

getInit :: EpistM -> [QState]
getInit (Mo _ _ val _ actual) = Q <$> map (\st -> Set.fromList . map fromForm . fromMaybe [] $ lookup st val) actual

buildMEStar :: EpistM -> EventModel -> RegularStructure Character QState
buildMEStar ep ev = RegularStructure 
    dAuto 
    [(ag, buildComposedSS ag ep ev dAuto) | ag <- agents ep]
    -- (pAutomata dAuto)
  where 
    dAuto = buildDAutomataNoF ep ev

propIncludes :: Agent -> Prop -> Bool
propIncludes ag (N i j) = i == ag || j == ag
propIncludes ag (S i j) = i == ag || j == ag

setEq :: Ord a => Set.Set a -> Set.Set a -> Bool
setEq m n = (m `Set.isSubsetOf` n) && (n `Set.isSubsetOf` m)


