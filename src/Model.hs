module Model where

import Data.Maybe
import Control.Applicative (liftA2)

newtype Agent = Ag Int deriving (Eq, Ord)

type World = Int
newtype State = State (World, [Event]) 
    deriving (Eq, Ord)
type Rel a = [[a]]
type AgentRel a = [(Agent, Rel a)]
type Valuation = [(State, [Form])]

-- TODO: Find a better way to do this
data Form = Top | P Prop | Not Form | And [Form] | Or [Form] | K Agent Form deriving (Eq, Show)

data Prop = S Agent Agent | N Agent Agent deriving (Eq, Show, Ord)

data EpistM = Mo {
    states :: [State],             -- Set of possible worlds
    agents :: [Agent],             -- Set of agents in model
    val :: Valuation,              -- Valuation function; \pi : World -> Set of props.
    eprel :: AgentRel State,     -- Epistemic relation between worlds
    actual :: [State]        -- Set of pointed worlds. 
    }

type PointedEpM = (EpistM, State)  -- This is a pointed model. 

-- So we want to be able to describe events; for us, we only have calls.
data Event = Call Agent Agent deriving (Eq, Ord)

-- We have event models = (E, R^E, pre, post).
-- pre is a function Event -> Form, whilst post is a function (Event, Prop) -> Form
data EventModel = EvMo {
    events :: [Event], 
    evrel :: AgentRel Event, 
    pre :: Precondition, 
    post :: Postcondition
}

type PointedEvM = (EventModel, Event)
type Precondition  = Event -> Form
type Postcondition = (Event, Prop) -> Form

-- We have an infinite amount of possible agents
-- But the first five get names!
a, b, c, d, e :: Agent
a = Ag 0; b = Ag 1; c = Ag 2; d = Ag 3; e = Ag 4

instance Show Agent where
    show (Ag 0) = "Ag a"; show (Ag 1) = "Ag b"
    show (Ag 2) = "Ag c"; show (Ag 3) = "Ag d"
    show (Ag 4) = "Ag e"
    show (Ag n) = "Ag " ++ show n

instance Show Event where
    show (Call i j) = show i ++ " " ++ show j 

instance Show State where
    show (State (w, [])) = "S " ++ show w
    show (State (w, es)) = "S " ++ show w ++ " Events: " ++ foldr ((++) . (++ ", ") . show) "." es

instance Show EpistM where
    show (Mo sts ags valuation erel initial) = 
        "States: " ++ show sts ++ "\n" ++
        "Agents: " ++ show ags ++ "\n" ++
        "Valuation: " ++ show valuation ++ "\n" ++ 
        "Relations: " ++ show erel ++ "\n" ++ 
        "Initial: " ++ show initial ++ "\n"

-- This lets us access the relations for a given agent
rel :: EpistM -> Agent -> Rel State
rel (Mo _ _ _ rels _) = table2fn rels

-- This gets the worlds related to world w
-- TODO: Perhaps change from head $
relatedWorlds :: Eq a => Rel a -> a -> [a]
relatedWorlds r w = concat $ filter (elem w) r

relatedWorldsAgent :: Eq a => AgentRel a -> Agent -> a -> [a]
relatedWorldsAgent r ag = relatedWorlds (fromMaybe [] (lookup ag r))

tval :: EpistM -> State -> [Form]
tval (Mo _ _ vals _ _) = table2fn vals

-- TODO: Consider changing default value to error?
table2fn :: Eq a => [(a, [b])] -> a -> [b]
table2fn t ag = fromMaybe [] (lookup ag t)

evaluateProp :: Prop -> EpistM -> State -> Bool
evaluateProp (N i j) m w 
    | i == j    = True
    | otherwise = P (N i j) `elem` tval m w
evaluateProp (S i j) m w 
    | i == j    = True
    | otherwise = P (S i j) `elem` tval m w



-- Give a semantics!
satisfies :: PointedEpM -> Form -> Bool
satisfies _ Top = True
satisfies (m, w) (P n) = evaluateProp n m w
satisfies (m, w) (Not p) = not $ satisfies (m, w) p
satisfies (m, w) (And ps) = all (satisfies (m, w)) ps 
satisfies (m, w) (Or ps) = any (satisfies (m, w)) ps
satisfies (m, w) (K ag p) = all (\v -> satisfies (m, v) p) rw 
  where 
    r = rel m ag
    rw = relatedWorlds r w

standardEventModel :: [Agent] -> Precondition -> Postcondition -> EventModel
standardEventModel ags = EvMo calls (callRel calls ags)
  where
    calls = [Call i j | i <- ags, j <- ags, i /= j]
    callRel evs ags = [(ag, unrel ag evs) | ag <- ags]
    unrel ag evs = [[ev] | ev <- evs, callIncludes ev ag] ++ [[ev | ev <- evs, not $ callIncludes ev ag]]

standardEpistModel :: [Agent] -> [Prop] -> EpistM
standardEpistModel ags fs = Mo
  [State (0, [])]
  ags
  [(State (0, []), map P fs)]
  (map (\ag -> (ag, [[State (0, [])]])) ags)
  [State (0, [])]

anyCall :: Precondition
anyCall (Call i j) = P (N i j)

callIncludes :: Event -> Agent -> Bool
callIncludes (Call i j) ag = (i == ag) || (j == ag)
-- callIncludes _ ag = False   -- In the case that we have any other events, what do we do? 

postUpdate :: Postcondition
postUpdate (Call i j, S n m) 
    | callIncludes (Call i j) n = Or [P (S i m), P (S j m)]
    | otherwise                 = P (S n m)
postUpdate (Call i j, N n m) 
    | callIncludes (Call i j) n = Or [P (N i m), P (N j m)]
    | otherwise                 = P (N n m)

produceAllProps :: [Agent] -> [Prop]
produceAllProps ags = [N i j | i <- ags, j <- ags, i /= j] ++ [S i j | i <- ags, j <- ags, i /= j]

update :: EpistM -> EventModel -> EpistM
update epm evm = 
    Mo states' (agents epm) val' rels' (actual epm)
    where
        states' = [stateUpdate s ev | s <- states epm, ev <- events evm, satisfies (epm, s) (pre evm ev)]
        rels' = [(ag, newRel ag) | ag <- agents epm]
        newRel agent = filterRel states' [liftA2 stateUpdate ss es |
                                          ss <- fromMaybe [] (lookup agent $ eprel epm),
                                          es <- fromMaybe [] (lookup agent $ evrel evm)] 
        val' = [(s, ps s) | s <- states']
        ps s = [P p | p <- props, satisfies (epm, trimLast s) (post evm (lastEv s, p))]
        props = produceAllProps $ agents epm



ptUpdate :: PointedEpM -> PointedEvM -> PointedEpM
ptUpdate (epModel, w) (evModel, ev) = (update epModel evModel, stateUpdate w ev)

filterRel :: Eq a => [a] -> Rel a -> Rel a
filterRel as = filter (not . null) . map (filter (`elem` as))

stateUpdate :: State -> Event -> State
stateUpdate (State (w, es)) ev = State (w, es ++ [ev])

allExperts :: EpistM -> Form 
allExperts (Mo _ ag _ _ _) = allExpertsAg ag

allExpertsAg :: [Agent] -> Form
allExpertsAg ag = And [P (S i j) | i <- ag, j <- ag, i /= j] 

lastEv :: State -> Event
lastEv (State (_, es)) = last es

trimLast :: State -> State
trimLast (State (w, es)) = State (w, init es)

stateRelPairs :: EpistM -> Agent -> [(State, State)]
stateRelPairs (Mo _ _ _ arel _) ag = relPairs $ fromMaybe [] (lookup ag arel) 

eventRelPairs :: EventModel -> Agent -> [(Event, Event)]
eventRelPairs ev ag = relPairs $ fromMaybe [] (lookup ag (evrel ev)) 

relPairs :: Rel a -> [(a, a)]
relPairs = concatMap allPairs
-- relPairs rel = concat $ [allPairs xs | xs <- rel]

allPairs :: [a] -> [(a, a)]
allPairs xs = concatMap (`pair` xs) xs

pair :: a -> [a] -> [(a, a)]
pair x ys = [(x, y) | y <- ys]












