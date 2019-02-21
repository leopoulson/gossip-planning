module ME where

import Model

data Character = World State | Call Agent Agent deriving (Eq, Show)
type Alphabet = [Character]

getAlphabet :: EpistM -> EventModel -> Alphabet
getAlphabet (Mo states _ _ _ _) (events, _, _, _) = map World states ++ map (\c@(Model.Call i j) -> ME.Call i j) events