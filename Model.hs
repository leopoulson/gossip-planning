module Model where

data Agent = Ag Int deriving (Eq, Ord)

a, b, c, d, e :: Agent
a = Ag 0; b = Ag 1; c = Ag 2; d = Ag 3; e = Ag 4

instance Show Agent where
    show (Ag 0) = "Agent a"; show (Ag 1) = "Agent b"
    show (Ag 2) = "Agent c"; show (Ag 3) = "Agent d"
    show (Ag 4) = "Agent e"
    show (Ag n) = "Agent " ++ show n