module ME where

import Model
import FSM
import Data.Maybe

-- data Character = World State | Call Agent Agent deriving (Eq, Show)
-- type Alphabet = [Character]

type Character' = Either State Event
type Alphabet' = [Character']

-- States in ME* are indexed just by the propositions that are true at them
-- So we can just let them *be* the propositions that are true at them
data QState = Q [Prop] | QInit

-- getAlphabet :: EpistM -> EventModel -> Alphabet
-- getAlphabet (Mo states _ _ _ _) (events, _, _, _) = map World states ++ map (\(Model.Call i j) -> ME.Call i j) events

getAlphabet' :: EpistM -> EventModel -> Alphabet'
getAlphabet' (Mo states _ _ _ _) (events, _, _, _) = map Left states ++ map Right events

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x : xs) = powerList xs ++ map (x:) (powerList xs)

getFStates :: EpistM -> [QState]
getFStates (Mo _ ags _ _ _ ) = fmap Q $ powerList $ produceAllProps ags

getQStates :: EpistM -> [QState]
getQStates mo = getFStates mo ++ [QInit]

getvee :: EpistM -> Valuation
getvee (Mo _ _ valu _ _) = valu

isForm :: Form -> Bool
isForm (P _) = True
isForm _ = False 

fromForm :: Form -> Prop
fromForm (P p) = p
fromForm _ = undefined

getForms :: [Form] -> [Prop]
getForms = map fromForm . filter isForm

meTrans :: [Agent] -> Valuation -> Precondition -> Postcondition -> Transition QState Character'
meTrans _   v _   _    (QInit, Left state)   = Q . getForms $ fromMaybe undefined (lookup state v)
meTrans _   _ _   _    (QInit, Right _)      = undefined -- Bad input
meTrans _   _ _   _    (Q _  , Left _)       = undefined -- Another baddie
meTrans ags _ pre post (Q ps , Right ev) 
    | not $ ps `models` pre ev               = undefined -- another baddie
    | otherwise                              = Q [p | p <- produceAllProps ags, ps `models` post (ev, p)]

models :: [Prop] -> Form -> Bool
models ps f = fromForm f `elem` ps











