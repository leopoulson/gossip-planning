module Model where

newtype Agent = Ag Int deriving (Eq, Ord)

-- We have an infinite amount of possible agents
-- But the first five get names!
a, b, c, d, e :: Agent
a = Ag 0; b = Ag 1; c = Ag 2; d = Ag 3; e = Ag 4

instance Show Agent where
    show (Ag 0) = "Agent a"; show (Ag 1) = "Agent b"
    show (Ag 2) = "Agent c"; show (Ag 3) = "Agent d"
    show (Ag 4) = "Agent e"
    show (Ag n) = "Agent " ++ show n

type World = Int
newtype State = State (World, [Event]) 
    deriving (Eq, Show, Ord)
type Rel a = [[a]]

-- TODO: Find a better way to do this
data Form = Top | P Prop | Not Form | And [Form] | Or [Form] | K Agent Form deriving (Eq, Show)
-- data FormK = Pr Form | K Agent Form

data Prop = S Agent Agent | N Agent Agent deriving (Eq, Show)

data EpistM = Mo 
    [State]           -- Set of possible worlds
    [Agent]           -- Set of agents in model
    [(State, [Form])]  -- Valuation function; \pi : World -> Set of props.
    [(Agent, Rel State)]    -- Epistemic relation between worlds
    [State]           -- Set of pointed worlds. 
    deriving (Show)

type PointedEpM = (EpistM, State)  -- This is a pointed model. 

example :: EpistM
example = Mo 
    [State (0, []), State (1, []), State (2, []), State (3, [])]
    [a, b, c]
    [(State (0, []), [P (S a b)]), (State (1, []), [P (S a b)]), (State (2, []), [P (S a b)])]
    [(a, [[State (0, [])], [State (1, [])], [State (2, [])], [State (3, [])]]), (b, [[State (0, [])], [State (1, [])], [State (2, [])], [State (3, [])]]), (c, [[State (0, []), State (1, []), State (2, [])], [State (3, [])]])]
    [State (1, [])]

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


-- Calls section

-- So we want to be able to describe events; for us, we only have calls.
data Event = Call Agent Agent deriving (Eq, Show, Ord)

-- We have event models = (E, R^E, pre, post).
-- pre is a function Event -> Form, whilst post is a function (Event, Prop) -> Form
type Precondition  = Event -> Form
type Postcondition = (Event, Prop) -> Form
-- So we can write a precondition like this!

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

type EventModel = ([Event], Rel Event, Precondition, Postcondition)
type PointedEvM = (EventModel, Event)

produceAllProps :: [Agent] -> [Prop]
produceAllProps ags = [N i j | i <- ags, j <- ags] ++ [S i j | i <- ags, j <- ags]

update :: EpistM -> PointedEvM -> EpistM
update m@(Mo states ag val rels actual) (evm@(es, erels, pre, post), e) = 
    Mo states' ag val' rels' actual
    where
        states' = [s | s <- states, satisfies (m, s) (pre e)]
        rels' = rels -- TODO: Change this? 
        val' = [(w, ps w) | w <- states']
        ps w = [P p | p <- props, satisfies (m, w) (post (e, p))]
        props = produceAllProps ag

update' :: EpistM -> EventModel -> EpistM
update' m@(Mo states ag val rels actual) (evm@(events, erels, pre, post)) = 
    Mo states' ag val' rels' actual
    where
        states' = [State (w, es ++ [e]) | s@(State (w, es)) <- states, e <- events , satisfies (m, s) (pre e)]
        rels' = rels -- [(a, newRel a) | a <- ag]
        -- newRel a = [[]]
        val' = [(State (w, es ++ [e]), ps s e) | s@(State (w, es)) <- states', e <- events]
        ps w e = [P p | p <- props, satisfies (m, w) (post (e, p))]
        props = produceAllProps ag

callExample :: EpistM
callExample = Mo 
    [State (0, [])] 
    [a, b] 
    [(State (0, []), [P (N a b), P (S a a), P (S b b)])] 
    [(a, [[State (0, [])]]), (b, [[State (0, [])]])] 
    [State (1, [])]

callEvM :: EventModel
callEvM = ([], [], anyCall, postUpdate)

allExperts :: EpistM -> Form 
allExperts (Mo _ ag _ _ _) = And [P (S i j) | i <- ag, j <- ag]
-- satisfies (update callExample (callEvM, Call a b), State (0, [])) (allExperts callExample)












