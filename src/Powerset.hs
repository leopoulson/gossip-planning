{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Powerset where

import Model
import FSM
import FST
import ME
import RS
import BFSM

import Data.List (nub, intersperse)
import Data.Maybe (fromJust, isJust)

import Debug.Trace

import qualified Data.Set as Set (toList, filter, fromList)

--import Data.Foldable hiding (concatMap, all, foldr, any)
--import Data.Monoid
--import Control.Applicative

type History ch st = [(ch, st)]
data PSH ch st = PSH (PState st) (History ch st)
data PState st = PList [PState st] | PCon (PState st) [PState st] | PVar st deriving (Eq, Show)

instance Functor PState where
    fmap f (PVar p) = PVar $ f p
    fmap f (PCon ps pss) = PCon (fmap f ps) (map (fmap f) pss)

instance Foldable PState where
    -- foldMap :: Monoid m => (a -> m) -> PState a -> m
    foldMap f (PVar p) = f p
    foldMap f (PCon p ps) = foldr (mappend . foldMap f) (foldMap f p) ps

instance Ord st => Ord (PState st) where
    PList pss1 `compare` PList pss2 = compare pss1 pss2
    PCon ps1 _ `compare` PCon ps2 _ = compare ps1 ps2
    PVar st1   `compare` PVar st2   = compare st1 st2
    compare _ _                     = error "Cannot compare items of different types"
    
{- There's a couple of things to sort out w.r.t this function.

 * The first is that we need some kind of guarantee that the agent whose 
   knowledge-formula we want to evaluate is the one that the current PSA
   is constructed for. This will probably require baking the agent into
   the type of either PState or the PSA itself. 
-}


instance (EvalState st p, Ord st) => EvalState (PState st) p where  
  evalState (K _ phi) (PCon _ sts) = all (evalState phi) sts
  evalState (K _ phi) (PList _)    = error "Can't evaluate K on a PList"
  evalState (K _ _) (PVar _)       = error "Can't evaluate K on a PVar"
  evalState (And phis) (PList ps)  = all (\pstate -> evalState (And phis) pstate) ps
  evalState (And phis) ps          = all (\phi -> evalState phi ps) phis
  evalState (Or phis) ps           = any (\phi -> evalState phi ps) phis
  evalState phi (PVar st)          = evalState phi st
  evalState phi (PCon st _)        = evalState phi st



ppPState :: Show p =>  Maybe (PState (QState p)) -> IO ()
ppPState (Just p) = putStr (prettyPrintPState p 0)
ppPState Nothing  = putStr "Nothing"

prettyPrintPState :: Show p => PState (QState p) -> Int -> String
prettyPrintPState (PVar (Q set)) n = concat (replicate n "    ") ++ "Var: " ++ (show $ Set.toList set) ++ "\n"
prettyPrintPState (PCon p ps)    n = concat (replicate n "    ") ++ "Con: \n" ++ prettyPrintPState p (n + 1) ++
                                     concat (replicate n "    ") ++ "Accessibles: \n" ++ concatMap (\p -> prettyPrintPState p (n+1)) ps
prettyPrintPState (PList ps)     n = concat (replicate n "    ") ++ "List: \n" ++
                                     concatMap (\p -> prettyPrintPState p (n + 1)) ps


-- Would it be possible to update the accepting states as we go along?
-- We might have to set the states after we've returned the PSA
-- Likewise for accepting; we don't know what the accepting states are
-- However, working with initial states here is perfect
buildPSA :: (Eq st, Show st, Show ch) => FSM ch (PState st) -> FST ch (PState st) -> TransFilter (PState st) ch -> FSM ch (PState st)
buildPSA fsm fstr filter = FSM alphabet' states' transition' initial' accepting' where
    alphabet'                = FSM.alphabet fsm
    accepting'               = error "Accepting not defined"
    states'                  = error "No states defined" 
    initial'                 = [PCon st [st] | st <- FSM.initial fsm]
    transition' (PCon state possStates, ch) = 
                case FSM.transition fsm (state, ch) of
                        Just st -> Just $ PCon st (getPossStates (state, ch) filter (bitransition fstr) possStates)
                        Nothing -> Nothing 
    transition' (PVar state, ch) = FSM.transition fsm (PVar state, ch)

getPossStates :: (Show st, Show ch, Eq st) => (st, ch) -> TransFilter st ch -> BiTransition st ch -> [st] -> [st]
-- getPossStates (st, ch) tfilter bitrans possStates = nub . concatMap (\stin -> map snd $ filter (
                                   -- (tfilter (st, ch))) $ bitrans (stin, ch)) $ possStates
-- getPossStates (st, ch) tfilter bitrans possStates = nub . concatMap (\stin -> map snd $ bitrans (stin, ch)) $ possStates
getPossStates (stbefore, ch) tfilter bitrans possStates = nub . concatMap (\stin -> map snd $ bitrans (stin, ch)) $
                                                          -- trace ("PossStates " ++ show possStates ++ "\n Filtered to " ++ show (filter (\stbefore' -> tfilter (stbefore, ch) (ch, stbefore')) possStates) ++ "\n\n")
                                                          filter (\stbefore' -> tfilter (stbefore, ch) (ch, stbefore')) possStates

psaFromScratch :: (Eq ev, Show ev, Prop p) => Agent -> EpistM (State ev) p -> EventModel ev p -> (Agent -> TransFilter (PState (QState p)) (Character ev)) -> FSM (Character ev) (PState (QState p))
psaFromScratch ag ep ev filt = buildPSA (makeP dAuto) (makePTrans $ buildComposedSS ag ep ev dAuto) (filt ag)
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
    trans' _    = error "No transition for a list"


makeP :: (Eq ev, Prop p) => FSM (Character ev) (QState p) -> FSM (Character ev) (PState (QState p))
makeP (FSM alpha sts trans int accept) = FSM alpha sts' trans' int' accept'
  where
    sts' = map PVar sts
    int' = map PVar int
    accept' (PVar st) = accept st
    accept' _         = error "Can't accept a PCon w/ a simple automata"
    trans' (PVar st, ch)     = PVar <$> trans (st, ch)
    trans' _ = error "No PCon states at this point"

makePTrans :: (Eq ev, Prop p) => FST (Character ev) (QState p) -> FST (Character ev) (PState (QState p))
makePTrans (FST alpha sts trans int accept) = FST alpha sts' trans' int' accept' 
  where
    sts' = map PVar sts
    int' = map PVar int
    accept' (PVar st) = accept st
    accept' _         = error "Can't accept a PCon w/ a simple automata"
    trans' (PVar st, ch)  = map (\(c, s) -> (c, PVar s)) $ trans (st, ch)
    trans' _              = error "No PCon states at this point"

-- findPath :: EvalState st => Form -> RegularStructure ch st -> Maybe [ch]
-- findPath (K _ _) _    = error "Can't find a path thru a regular structure with K"
-- findPath phi rs       = extractCalls . doBFS $ setSuccessfulFormula phi (dAutomata rs)

-- for buildPSA, our dAutomata and our transducer are on the same "level"
-- hence we lift the transducers to the level of dAuto. 
-- Then our other transducers sit the level below the PSA, that is the level of dAuto
-- buildSolveRS :: Form -> RegularStructure ch (PState QState) -> RegularStructure ch (PState QState)
-- buildSolveRS (K ag phi) rs = RegularStructure 
    -- (buildPSA (dAutomata $ buildSolveRS phi rs) (liftTransducer (dAutomata $ buildSolveRS phi rs) $ getTransducer ag rs) (knowFilter ag))
    -- (liftTransducers (dAutomata $ buildSolveRS phi rs) $ transducers rs)
-- buildSolveRS phi rs = rs
  -- where
    -- dAuto = dAutomata $ buildSolveRS phi rs

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

fromPState :: PState st -> st
fromPState (PVar st) = st
fromPState (PCon st _) = fromPState st

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
-- This is screaming out for a typeclass. This would make all of our worries go away
createSolvingAutomata :: (Eq ev, Show ev, Prop p) => Form p -> EpistM (State ev) p -> EventModel ev p -> (Agent -> TransFilter (PState (QState p)) (Character ev)) -> FSM (Character ev) (PState (QState p))
createSolvingAutomata form@(K agent phi) ep ev tfilter = setStatesReachableInit $ setSuccessfulFormula form $
                          buildPSA (createSolvingAutomata phi ep ev tfilter) (buildComposedSS agent ep ev (createSolvingAutomata phi ep ev tfilter)) (tfilter agent)
createSolvingAutomata (And phis) ep ev tfilter        = case includesK (And phis) of
  True  -> toPList $ intersectionFSM $ map (\phi -> createSolvingAutomata phi ep ev tfilter) phis
  False -> makeP $ buildDAutomata (And phis) ep ev
createSolvingAutomata (Or phis) ep ev tfilter        = case includesK (Or phis) of
  True  -> toPList $ unionFSM $ map (\phi -> createSolvingAutomata phi ep ev tfilter) phis
  False -> makeP $ buildDAutomata (Or phis) ep ev
createSolvingAutomata phi ep ev _ = makeP $ buildDAutomata phi ep ev

includesK :: Form p -> Bool
includesK (K _ _)  = True
includesK (And ps) = any includesK ps
includesK (Or ps)  = any includesK ps
includesK (Not p)  = includesK p
includesK (P _)    = False
includesK Top      = False

toPList :: (Eq ev, Prop p) => FSM (Character ev) [PState (QState p)] -> FSM (Character ev) (PState (QState p))
toPList (FSM alpha _ trans int accept) = FSM alpha states' trans' int' accept'
  where
    --alpha = FSM.alphabet fsms
    states' = undefined
    trans' (PList ps, ch) = PList <$> trans (ps, ch)
    trans' _              = error "Only transition for PLists"
    int' = PList <$> int
    accept' (PList ps) = accept ps 
    accept' _          = error "Can't accept a non-PList"


-- findCallSequence :: Form GosProp -> EpistM StateC GosProp -> EventModel Call GosProp -> Maybe [CallChar]
-- findCallSequence form ep ev = extractCalls . doBFS $ createSolvingAutomata form ep ev

propIncludes :: Agent -> GosProp -> Bool
propIncludes ag (N i j) = i == ag || j == ag
propIncludes ag (S i j) = i == ag || j == ag

isKnowledge :: Agent -> GosProp -> Bool
isKnowledge ag (N i j) = i == ag
isKnowledge ag (S i j) = i == ag

knowFilter :: Agent -> TransFilter (PState (QState GosProp)) CallChar
knowFilter ag (PVar (Q qs), Right (Call i j)) (Right (Call i' j'), PVar (Q ps))
  -- | i == i' && j == j' && (i == ag || j == ag) =
      -- Set.filter (isKnowledge ag) qs `setEq` Set.filter (isKnowledge ag) ps
  | i == i' && j == j' && (i == ag) =
      Set.filter (isKnowledge j) qs `setEq` Set.filter (isKnowledge j) ps
  | i == i' && j == j' && (j == ag) =
      Set.filter (isKnowledge i) qs `setEq` Set.filter (isKnowledge i) ps
  | i == ag || j == ag = error "knowFilter: If I or J are the agent, the calls must be equal"
  | i /= ag && j /= ag = True
  | otherwise          = error ("Unmatched case in knowFilter: " ++ show ((Call i j)) ++ " " ++ show ((Call i' j')))
-- I haven't really thought about what to do here yet.
-- Perhaps mimic something similar to above, where we just scope down into the underlying truth?
knowFilter ag (PCon sta stas, ca) (cb, PCon stb stbs) = knowFilter ag (point sta, ca) (cb, point stb)
knowFilter ag (PList a, ca) (cb, PList b) = True
