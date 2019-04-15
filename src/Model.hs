module Model where

import Debug.Trace
import Data.Maybe
import Control.Applicative (liftA2)

newtype Agent = Ag Int deriving (Eq)

type World = Int

newtype State ev = State (World, [ev]) 
    deriving (Eq, Ord)

type StateC = State Call

type Rel a = [[a]]
type AgentRel a = [(Agent, Rel a)]
type Valuation st prp = [(st, [Form prp])]

data Form prp = Top | P prp | Not (Form prp) | And [(Form prp)] | Or [(Form prp)] | K Agent (Form prp) deriving (Eq, Show)

instance Functor (Form) where
  fmap f Top = Top
  fmap f (P p) = P (f p)
  fmap f (Not form) = Not (f <$> form)
  fmap f (And forms) = And (map (f <$>) forms)
  fmap f (Or forms) = Or (map (f <$>) forms)
  fmap f (K ag form) = K ag (f <$> form)

data EpistM st p = Mo {
    states :: [st],                  -- Set of possible worlds
    agents :: [Agent],               -- Set of agents in model
    val :: Valuation st p,             -- Valuation function; \pi : World -> Set of props.
    eprel :: AgentRel st,            -- Epistemic relation between worlds
    actual :: [st],                  -- Set of pointed worlds.
    allProps :: [p] 
    }

type PointedEpM st p = (EpistM st p, st)  -- This is a pointed model. 

-- So we want to be able to describe events; for us, we only have calls.
data Call = Call Agent Agent deriving (Eq)

-- We have event models = (E, R^E, pre, post).
-- pre is a function Event -> Form, whilst post is a function (Event, Prop) -> Form
data EventModel ev prp = EvMo {
    events :: [ev],
    evrel :: AgentRel ev,
    pre :: Precondition ev prp,
    post :: Postcondition ev prp
}

type PointedEvM ev prp = (EventModel ev prp, ev)
type Precondition ev prp = ev -> Form prp
type Postcondition ev prp = (ev, prp) -> Form prp

-- We have an infinite amount of possible agents
-- But the first five get names!
a, b, c, d, e :: Agent
a = Ag 0; b = Ag 1; c = Ag 2; d = Ag 3; e = Ag 4

instance Show Agent where
    show (Ag 0) = "Ag a"; show (Ag 1) = "Ag b"
    show (Ag 2) = "Ag c"; show (Ag 3) = "Ag d"
    show (Ag 4) = "Ag e"
    show (Ag n) = "Ag " ++ show n

instance Show Call where
    show (Call i j) = show i ++ " " ++ show j 

instance Show ev => Show (State ev) where
    show (State (w, [])) = "S " ++ show w
    show (State (w, es)) = "S " ++ show w ++ " Events: " ++ foldr ((++) . (++ ", ") . show) "." es

instance (Show st, Show p) => Show (EpistM st p) where
    show (Mo sts ags valuation erel initial allprops) = 
        "States: " ++ show sts ++ "\n" ++
        "Agents: " ++ show ags ++ "\n" ++
        "Valuation: " ++ show valuation ++ "\n" ++ 
        "Relations: " ++ show erel ++ "\n" ++ 
        "Initial: " ++ show initial ++ "\n" ++
        "All props: " ++ show allprops ++ "\n"

instance Ord Agent where
    Ag n `compare` Ag m = n `compare` m

instance Ord GosProp where
    (S a1 a2) `compare` (S b1 b2) = if (a1 == b1) then a2 `compare` b2 else a1 `compare` b1
    (N a1 a2) `compare` (N b1 b2) = if (a1 == b1) then a2 `compare` b2 else a1 `compare` b1
    (N _ _)   `compare` (S _ _)   = LT
    (S _ _)   `compare` (N _ _)   = GT

instance Ord Call where
    (Call a1 a2) `compare` (Call b1 b2) = if (a1 == b1) then a2 `compare` b2 else a1 `compare` b1

-- This typeclass guarantees us that the thing that the propositions we're handling can be evaluated
class (Show p, Ord p) => Prop p where
  evalProp :: Eq st => p -> EpistM st p -> st -> Bool
  allProps' :: [Agent] -> [p]

data GosProp = S Agent Agent | N Agent Agent deriving (Eq, Show)

instance Prop GosProp where
    evalProp (N i j) m w 
      | i == j    = True
      | otherwise = P (N i j) `elem` tval m w
    evalProp (S i j) m w 
      | i == j    = True
      | otherwise = P (S i j) `elem` tval m w
    allProps' ags = [N i j | i <- ags, j <- ags] ++ [S i j | i <- ags, j <- ags]


-- This lets us access the relations for a given agent
rel :: EpistM st p -> Agent -> Rel st
rel (Mo _ _ _ rels _ _) = table2fn rels

-- This gets the worlds related to world w
-- TODO: Perhaps change from head $
relatedWorlds :: Eq a => Rel a -> a -> [a]
relatedWorlds r w = concat $ filter (elem w) r

relatedWorldsAgent :: Eq a => AgentRel a -> Agent -> a -> [a]
relatedWorldsAgent r ag = relatedWorlds (fromMaybe [] (lookup ag r))

tval :: Eq st => EpistM st p -> st -> [Form p]
tval (Mo _ _ vals _ _ _) = table2fn vals

-- TODO: Consider changing default value to error?
table2fn :: Eq a => [(a, [b])] -> a -> [b]
table2fn t ag = fromMaybe [] (lookup ag t)

-- well nice, now we don't need to worrya bout what it particularly "is"
satisfies :: (Eq st, Prop p) => PointedEpM st p -> Form p -> Bool 
satisfies _ Top = True
satisfies (m, w) (P n) = evalProp n m w
satisfies (m, w) (Not p) = not $ satisfies (m, w) p
satisfies (m, w) (And ps) = all (satisfies (m, w)) ps 
satisfies (m, w) (Or ps) = any (satisfies (m, w)) ps
satisfies (m, w) (K ag p) = all (\v -> satisfies (m, v) p) rw 
  where 
    r = rel m ag
    rw = relatedWorlds r w

standardEventModel :: [Agent] -> Precondition Call GosProp -> Postcondition Call GosProp -> EventModel Call GosProp
standardEventModel ags = EvMo calls (callRel calls ags)
  where
    calls = [Call i j | i <- ags, j <- ags, i /= j]
    callRel evs ags = [(ag, unrel ag evs) | ag <- ags]
    unrel ag evs = [[ev] | ev <- evs, callIncludes ev ag] ++ [[ev | ev <- evs, not $ callIncludes ev ag]]

midProps :: [Agent] -> [GosProp]
midProps ags = [S i i | i <- ags] ++ [N i i | i <- ags]

standardEpistModel :: [Agent] -> [GosProp] -> EpistM StateC GosProp
standardEpistModel ags fs = Mo
  [State (0, [])]
  ags
  [(State (0, []), map P fs ++ map P (midProps ags))]
  (map (\ag -> (ag, [[State (0, [])]])) ags)
  [State (0, [])]
  (produceAllProps ags)

anyCall :: Precondition Call GosProp
anyCall (Call i j) = P (N i j)

-- We may only call someone if we do not know
lns :: Precondition Call GosProp
lns (Call i j) = And [Not (P (S i j)), P (N i j)]

callIncludes :: Call -> Agent -> Bool
callIncludes (Call i j) ag = (i == ag) || (j == ag)
-- callIncludes _ ag = False   -- In the case that we have any other events, what do we do? 

postUpdate :: Postcondition Call GosProp
postUpdate (Call i j, S n m) 
    | callIncludes (Call i j) n = Or [P (S i m), P (S j m)]
    | otherwise                 = P (S n m)
postUpdate (Call i j, N n m) 
    | callIncludes (Call i j) n = Or [P (N i m), P (N j m)]
    | otherwise                 = P (N n m)

produceAllProps :: [Agent] -> [GosProp]
produceAllProps ags = [N i j | i <- ags, j <- ags] ++ [S i j | i <- ags, j <- ags]

update :: EpistM StateC GosProp -> EventModel Call GosProp -> EpistM StateC GosProp
update epm evm = 
    Mo states' (agents epm) val' rels' (actual epm) (allProps epm)
    where
        states' = [stateUpdate s ev | s <- states epm, ev <- events evm, satisfies (epm, s) (pre evm ev)]
        rels' = [(ag, newRel ag) | ag <- agents epm]
        newRel agent = filterRel states' [liftA2 stateUpdate ss es |
                                          ss <- fromMaybe [] (lookup agent $ eprel epm),
                                          es <- fromMaybe [] (lookup agent $ evrel evm)] 
        val' = [(s, ps s) | s <- states']
        ps s = [P p | p <- props, satisfies (epm, trimLast s) (post evm (lastEv s, p))]
        props = produceAllProps $ agents epm

update' :: (Eq ev, Prop p) => EpistM (State ev) p -> EventModel ev p -> EpistM (State ev) p
update' epm evm = Mo states' (agents epm) val' rels' (actual epm) (allProps epm)
  where
    states' = [stateUpdate s ev | s <- states epm, ev <- events evm, satisfies (epm, s) (pre evm ev)]
    rels' = [(ag, newRel ag) | ag <- agents epm]
    newRel agent = filterRel states' [liftA2 stateUpdate ss es |
                                      ss <- fromMaybe [] (lookup agent $ eprel epm),
                                      es <- fromMaybe [] (lookup agent $ evrel evm)]
    val' = [(s, ps s) | s <- states']
    ps s = [P p | p <- props, satisfies (epm, trimLast s) (post evm (lastEv s, p))]
    props = allProps' (agents epm)

updateSingle :: (Eq ev, Prop p, Show ev) => EpistM (State ev) p -> PointedEvM ev p -> EpistM (State ev) p
updateSingle epm (evm, ev) = Mo states' (agents epm) val' rels' (actual epm) (allProps epm)
  where
    states' = [stateUpdate s ev | s <- states epm, satisfies (epm, s) (pre evm ev)]
    rels' = [(ag, newRel ag) | ag <- agents epm]
    newRel agent = filterRel states' [liftA2 stateUpdate ss es |
                                      ss <- fromMaybe [] (lookup agent $ eprel epm),
                                      es <- fromMaybe [] (lookup agent $ evrel evm)]
    val' = [(s, ps s) | s <- states']
    ps s = [P p | p <- props, satisfies (epm, trimLast s) (post evm (lastEv s, p))]
    props = allProps epm

lastEv :: State ev -> ev
lastEv (State (_, es)) = last es

trimLast :: State ev -> State ev
trimLast (State (w, es)) = State (w, init es)

ptUpdate :: (Eq ev, Prop p) => PointedEpM (State ev) p -> PointedEvM ev p -> PointedEpM (State ev) p
ptUpdate (epModel, w) (evModel, ev) = (update' epModel evModel, stateUpdate w ev)

filterRel :: Eq a => [a] -> Rel a -> Rel a
filterRel as = filter (not . null) . map (filter (`elem` as))

stateUpdate :: State ev -> ev -> State ev
stateUpdate (State (w, es)) ev = State (w, es ++ [ev])

allExperts :: EpistM StateC GosProp -> Form GosProp
allExperts (Mo _ ag _ _ _ _) = allExpertsAg ag

allExpertsAg :: [Agent] -> Form GosProp
allExpertsAg ag = And [P (S i j) | i <- ag, j <- ag, i /= j] 

stateRelPairs :: EpistM st pr -> Agent -> [(st, st)]
stateRelPairs (Mo _ _ _ arel _ _) ag = relPairs $ fromMaybe [] (lookup ag arel) 

eventRelPairs :: EventModel ev p -> Agent -> [(ev, ev)]
eventRelPairs ev ag = relPairs $ fromMaybe [] (lookup ag (evrel ev)) 

relPairs :: Rel a -> [(a, a)]
relPairs = concatMap allPairs
-- relPairs rel = concat $ [allPairs xs | xs <- rel]

allPairs :: [a] -> [(a, a)]
allPairs xs = concatMap (`pair` xs) xs

pair :: a -> [a] -> [(a, a)]
pair x ys = [(x, y) | y <- ys]



