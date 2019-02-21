module Model where

import Data.Maybe
import Control.Applicative

newtype Agent = Ag Int deriving (Eq, Ord)

type World = Int
newtype State = State (World, [Event]) 
    deriving (Eq, Ord)
type Rel a = [[a]]
type AgentRel a = [(Agent, Rel a)]

-- TODO: Find a better way to do this
data Form = Top | P Prop | Not Form | And [Form] | Or [Form] | K Agent Form deriving (Eq, Show)

data Prop = S Agent Agent | N Agent Agent deriving (Eq, Show)

data EpistM = Mo 
    [State]           -- Set of possible worlds
    [Agent]           -- Set of agents in model
    [(State, [Form])]  -- Valuation function; \pi : World -> Set of props.
    (AgentRel State)    -- Epistemic relation between worlds
    [State]           -- Set of pointed worlds. 

type PointedEpM = (EpistM, State)  -- This is a pointed model. 

-- So we want to be able to describe events; for us, we only have calls.
data Event = Call Agent Agent deriving (Eq, Ord)

-- We have event models = (E, R^E, pre, post).
-- pre is a function Event -> Form, whilst post is a function (Event, Prop) -> Form
type EventModel = ([Event], AgentRel Event, Precondition, Postcondition)
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
    show (Mo states agents valuation erel initial) = 
        "States: " ++ show states ++ "\n" ++
        "Agents: " ++ show agents ++ "\n" ++
        "Valuation: " ++ show valuation ++ "\n" ++ 
        "Relations: " ++ show erel ++ "\n" ++ 
        "Initial: " ++ show initial ++ "\n"

-- This lets us access the relations for a given agent
rel :: EpistM -> Agent -> Rel State
rel (Mo _ _ _ rels _) ag = table2fn rels ag

-- This gets the worlds related to world w
-- TODO: Perhaps change from head $
relatedWorlds :: Rel State -> State -> [State]
relatedWorlds r w = concat $ filter (elem w) r

val :: EpistM -> State -> [Form]
val (Mo _ _ vals _ _) st = table2fn vals st

-- TODO: Consider changing default value to error?
table2fn :: Eq a => [(a, [b])] -> a -> [b]
table2fn t ag = maybe [] id (lookup ag t)

-- Give a semantics!
satisfies :: PointedEpM -> Form -> Bool
satisfies _ Top = True
satisfies (m, w) (P n) = P n `elem` val m w
satisfies (m, w) (Not p) = not $ satisfies (m, w) p
satisfies (m, w) (And ps) = and $ map (\p -> satisfies (m, w) p) ps--(satisfies (m, w) p) && (satisfies (m, w) q)
satisfies (m, w) (Or ps) = or $ map (\p -> satisfies (m, w) p) ps
satisfies (m, w) (K ag p) = all (\v -> satisfies (m, v) p) rw 
  where 
    r = rel m ag
    rw = relatedWorlds r w

anyCall :: Precondition
anyCall (Call i j) = P (N i j)

callIncludes :: Event -> Agent -> Bool
callIncludes (Call i j) ag = (i == ag) || (j == ag)
-- callIncludes _ ag = False   -- In the case that we have any other events, what do we do? 

-- This doesn't work; fix it
postUpdate :: Postcondition
postUpdate (Call i j, S n m) 
    | callIncludes (Call i j) n = Or [P (S i m), P (S j m)]
    | otherwise                 = P (S n m)
postUpdate (Call i j, N n m) 
    | callIncludes (Call i j) n = Or [P (N i m), P (N j m)]
    | otherwise                 = P (N n m)

produceAllProps :: [Agent] -> [Prop]
produceAllProps ags = [N i j | i <- ags, j <- ags] ++ [S i j | i <- ags, j <- ags]

update :: EpistM -> EventModel -> EpistM
update m@(Mo states ags _ rels actual) (events, erels, pre, post) = 
    Mo states' ags val' rels' actual
    where
        states' = [stateUpdate s ev | s <- states, ev <- events , satisfies (m, s) (pre ev)]
        rels' = [(ag, newRel ag) | ag <- ags]
        newRel agent = [liftA2 stateUpdate ss es | ss <- fromMaybe [] (lookup agent rels), es <- fromMaybe [] (lookup agent erels)]
        val' = [(s, ps s) | s <- states']
        ps s = [P p | p <- props, satisfies (m, trimLast s) (post (lastEv s, p))]
        props = produceAllProps ags

stateUpdate :: State -> Event -> State
stateUpdate (State (w, es)) ev = State (w, es ++ [ev])

allExperts :: EpistM -> Form 
allExperts (Mo _ ag _ _ _) = And [P (S i j) | i <- ag, j <- ag]

lastEv :: State -> Event
lastEv (State (_, es)) = last es

trimLast :: State -> State
trimLast (State (w, es)) = State (w, init es)










