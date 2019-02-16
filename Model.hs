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

data Prop = Top | P Char | Not Prop | And Prop Prop 

data PropK = Prop | K Agent Prop

data EpistM = Mo 
    [World]              -- Set of possible worlds
    [Agent]              -- Set of agents in model
    [(World, [Prop])]    -- Valuation function; \pi : World -> Set of props.
    [(Agent, [[World]])] -- Epistemic relation between worlds
    [World]              -- Set of initial worlds. 








