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
  evalState (K _ phi) (PCon _ sts) = all (evalState phi) sts
  evalState (K _ _) (PVar _)       = error "Can't evaluate K on a PVar"
  evalState (And phis) ps          = all (\phi -> evalState phi ps) phis
  evalState (Or phis) ps           = any (\phi -> evalState phi ps) phis
  evalState phi (PVar st)          = evalState phi st
  evalState phi (PCon st _)        = evalState phi st
  
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
    dAuto = buildDAutomataNoF ep ev

makeSingleton :: FSM ch st -> FSM ch [st]
makeSingleton (FSM alpha sts trans int accept) = FSM alpha sts' trans' int' accept'
  where
    sts' = map (: []) sts
    int' = map (: []) int
    accept' [st] = accept st
    accept' _    = error "Can't accept a list of calls"
    trans' ([st], ch) = (: []) <$> trans (st, ch)
    trans' (_, ch)    = error "No transition for a list"


makeP :: FSM Character QState -> FSM Character (PState QState)
makeP (FSM alpha sts trans int accept) = FSM alpha sts' trans' int' accept'
  where
    sts' = map PVar sts
    int' = map PVar int
    accept' (PVar st) = accept st
    accept' _         = error "Can't accept a PCon w/ a simple automata"
    trans' (PVar st, ch)     = PVar <$> trans (st, ch)
    trans' (PCon st sts, ch) = error "No PCon states at this point"

makePTrans :: FST Character QState -> FST Character (PState QState)
makePTrans (FST alpha sts trans int accept) = FST alpha sts' trans' int' accept' 
  where
    sts' = map PVar sts
    int' = map PVar int
    accept' (PVar st) = accept st
    accept' _         = error "Can't accept a PCon w/ a simple automata"
    trans' (PVar st, ch)     = map (\(c, s) -> (c, PVar s)) $ trans (st, ch)
    trans' (PCon st sts, ch) = error "No PCon states at this point"

findPath :: EvalState st => Form -> RegularStructure ch st -> Maybe [ch]
findPath (K a phi) rs = undefined 
findPath phi rs       = extractCalls . doBFS $ setSuccessfulFormula phi (dAutomata rs)

-- -- lens time
-- for buildPSA, our dAutomata and our transducer are on the same "level"
-- hence we lift the transducers to the level of dAuto. 
-- Then our other transducers sit the level below the PSA, that is the level of dAuto
buildSolveRS :: EvalState st => Form -> RegularStructure ch (PState st) -> RegularStructure ch (PState st)
buildSolveRS (K ag phi) rs = RegularStructure 
    (buildPSA (dAutomata $ buildSolveRS phi rs) (liftTransducer (dAutomata $ buildSolveRS phi rs) $ getTransducer ag rs))
    (liftTransducers (dAutomata $ buildSolveRS phi rs) $ transducers rs)
buildSolveRS phi rs = rs
  where
    dAuto = dAutomata $ buildSolveRS phi rs

liftTransducers :: FSM ch (PState st) -> [(Agent, FST ch (PState st))] -> [(Agent, FST ch (PState st))]
liftTransducers fsm = map (\(ag, tr) -> (ag, liftTransducer fsm tr))

-- Lol this isn't very good. Need to think more about this
liftTransducer :: FSM ch (PState st) -> FST ch (PState st) -> FST ch (PState st)
liftTransducer fsm (FST _ _ tr _ _) = FST undefined undefined (liftTransition tr fsm) undefined undefined

pointedWorld :: PState st -> st
pointedWorld (PCon st _) = pointedWorld st
pointedWorld (PVar st)   = st

-- Here we first want to find the set of calls that are possible at our current state.
-- We use point pstate to get the real world, and then find the indistinguishable calls from here.
-- Then from here, we can use the FSM given to find the other worlds we can transition to. 
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

-- For the K case, we want to perform some operation on the result of the function for phi.
-- Most likely, we will call buildSolveRS? And then extract some information somehow
-- We really need to consider what to do when we get a formula like
--         K a phi AND K b psi
-- Can we just look at the modalities and see what it is that we need to attach on?
-- Or is it deeper than this? Do we need to create the automata on the fly?
-- Or just when we get an and behave differently? 
createSolvingAutomata :: Form -> EpistM -> EventModel -> FSM Character (PState QState)
createSolvingAutomata form@(K agent phi) ep ev = setStatesReachableInit $ setSuccessfulFormula form $
                                                 buildPSA (createSolvingAutomata phi ep ev) (buildComposedSS agent ep ev (createSolvingAutomata phi ep ev))
--createSolvingAutomata (And phis) ep ev         = intersectionFSM $ map (\phi -> createSolvingAutomata phi ep ev) phis
createSolvingAutomata phi                ep ev = makeP $ buildDAutomata phi ep ev
  where
    -- lowerAuto :: FSM Character (PState QState)
    lowerAuto = createSolvingAutomata phi ep ev





 
