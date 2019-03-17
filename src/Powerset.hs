module Powerset where

import Model
import FSM
import FST
import ME
import RS
import BFSM

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

type History ch st = [(ch, st)]
data PSH ch st = PSH (PState st) (History ch st)
data PState st = PCon (PState st) [PState st] | PVar st deriving (Eq, Show)

instance Functor PState where
    fmap f (PVar p) = PVar $ f p 
    fmap f (PCon ps pss) = PCon (fmap f ps) (map (fmap f) pss)

instance Foldable PState where
    -- foldMap :: Monoid m => (a -> m) -> PState a -> m
    foldMap f (PVar p) = f p
    foldMap f (PCon p ps) = foldr (mappend . foldMap f) (foldMap f p) ps

{- There's a couple of things to sort out w.r.t this function.

 * The first is that we need some kind of guarantee that the agent whose 
   knowledge-formula we want to evaluate is the one that the current PSA
   is constructed for. This will probably require baking the agent into
   the type of either PState or the PSA itself. 
-}
instance (EvalState st, Eq st) => EvalState (PState st) where
  -- evalState (K _ p)  (PCon _ sts) = all (evalState p) sts
  -- evalState f        (PVar st)  = evalState f st
  evalState p = all (evalState p) 

-- Would it be possible to update the accepting states as we go along?
-- We might have to set the states after we've returned the PSA
-- Likewise for accepting; we don't know what the accepting states are
-- However, working with initial states here is perfect
buildPSA :: EvalState st => FSM ch (PState st) -> FST ch (PState st) -> FSM ch (PState st)
buildPSA fsm fstr = FSM alphabet' states' transition' initial' accepting' where
    alphabet'                = FSM.alphabet fsm
    accepting'               = error "Accepting not defined"
    states'                  = error "No states defined" -- hmmm what to do here? explicitly list the states? give a 'well-formed' function?
    initial'                 = [PCon st [st] | st <- FSM.initial fsm]
    transition' (PCon state possStates, ch) = 
                case FSM.transition fsm (state, ch) of   --Probably update this to fmap eventually 
                        Just st -> Just $ PCon st (getPossStates ch possStates)
                        Nothing -> Nothing 
    transition' (PVar state, ch) = FSM.transition fsm (PVar state, ch)
    getPossStates ch = nub . concatMap (\st -> map snd $ bitransition fstr (st, ch))

psaFromScratch :: Agent -> EpistM -> EventModel -> FSM Character (PState QState)
psaFromScratch ag ep ev = buildPSA (makeP dAuto) (makePTrans $ buildComposedSS ag ep ev dAuto)
  where
    dAuto = buildDAutomata ep ev

makePTrans :: FST Character QState -> FST Character (PState QState)
makePTrans (FST alpha sts trans int accept) = FST alpha sts' trans' int' accept' 
  where
    sts' = map PVar sts
    int' = map PVar int
    accept' (PVar st) = accept st
    accept' _         = error "Can't accept a PCon w/ a simple automata"
    trans' (PVar st, ch)     = map (\(c, s) -> (c, PVar s)) $ trans (st, ch)
    trans' (PCon st sts, ch) = error "No PCon states at this point"

makeP :: FSM Character QState -> FSM Character (PState QState)
makeP (FSM alpha sts trans int accept) = FSM alpha sts' trans' int' accept'
  where
    sts' = map PVar sts
    int' = map PVar int
    accept' (PVar st) = accept st
    accept' _         = error "Can't accept a PCon w/ a simple automata"
    trans' (PVar st, ch)     = PVar <$> trans (st, ch)
    trans' (PCon st sts, ch) = error "No PCon states at this point"


setSuccessfulFormula :: EvalState st => Form -> FSM ch st -> FSM ch st
setSuccessfulFormula f = updateAcccepting (evalState f) 

findPath :: EvalState st => Form -> RegularStructure ch st -> Maybe [ch]
findPath (K a phi) rs = undefined 
findPath phi rs       = extractCalls . doBFS $ setSuccessfulFormula phi (dAutomata rs)

-- -- lens time
-- for buildPSA, our dAutomata and our transducer are on the same "level"
-- hence we lift the transducers to the level of dAuto. 
-- Then our other transducers sit the level below the PSA, that is the level of dAuto
buildSolveAutomata :: EvalState st => Form -> RegularStructure ch (PState st) -> RegularStructure ch (PState st)
buildSolveAutomata (K ag phi) rs = RegularStructure 
    (buildPSA (dAutomata $ buildSolveAutomata phi rs) (liftTransducer (dAutomata $ buildSolveAutomata phi rs) $ getTransducer ag rs))
    (liftTransducers (dAutomata $ buildSolveAutomata phi rs) $ transducers rs)
buildSolveAutomata phi rs = rs
  where
    dAuto = dAutomata $ buildSolveAutomata phi rs

liftTransducers :: FSM ch (PState st) -> [(Agent, FST ch (PState st))] -> [(Agent, FST ch (PState st))]
liftTransducers fsm = map (\(ag, tr) -> (ag, liftTransducer fsm tr))

-- Lol this isn't very good. Need to think more about this
liftTransducer :: FSM ch (PState st) -> FST ch (PState st) -> FST ch (PState st)
liftTransducer fsm (FST _ _ tr _ _) = FST undefined undefined (liftTransition tr fsm) undefined undefined

pointedWorld :: PState st -> st
pointedWorld (PCon st _) = pointedWorld st
pointedWorld (PVar st)   = st

-- To lift the transducer transition, we want to find the PStates with the
-- set of calls that are indistinguishable from one another.. 
-- This is going to entail checking the calls that we took to get to our current state, 
-- then relating them with other calls, and then taking those call steps through the
-- automata. 
-- It's unclear how we can extract the sequence of calls from the current state, 
-- so we will add the calls into the type. 
-- Note that the FSM here is the FSM from the lifted level
liftTransition :: BiTransition (PState st) ch -> FSM ch (PState st) -> BiTransition (PState st) ch
liftTransition trans fsm (pstate, ch) = fromSndMaybe [(ch', transition fsm (pstate, ch')) | ch' <- possCalls trans (pstate, ch)]
  where
    possCalls :: BiTransition (PState st) ch -> (PState st, ch) -> [ch]
    possCalls trans (pstate, ch) = fst <$> trans (point pstate, ch)

-- Given that this is being called from liftTransition, it's hard to imagine a case
-- in which point will be called on a PVar; our lifted states will just be 
-- PCons and so on.
-- Perhaps it would be worth further investigating if this is true 
point :: PState st -> PState st
point (PCon st _) = st
point (PVar st)   = PVar st

fromSndMaybe :: [(a, Maybe b)] -> [(a, b)]
fromSndMaybe = map (\(l, r) -> (l, fromJust r)) . 
               filter (isJust . snd)

-- liftTransition :: BiTransition st ch -> BiTransition (PState st) ch
-- liftTransition trans (PCon st sts, ch) = 
--     map (\(ch', s) -> (ch', PCon s sts)) $ trans (pointedWorld st, ch)
-- liftTransition trans (PVar st, ch) =
--     map (\(ch', s) -> (ch', PVar s)) $ trans (st, ch)







