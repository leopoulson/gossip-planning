module RS where

import FSM 
import FST
import Model

import Data.Maybe (fromMaybe)

-- Definition for transducers is a bit unsatisfactory
data RegularStructure ch st = RegularStructure {
    dAutomata :: FSM ch st, 
    transducers :: [(Agent, FST ch st)], 
    fAutomata :: Prop -> FSM ch st
}

getTransducer :: Agent -> RegularStructure ch st -> FST ch st
getTransducer agent = fromMaybe (error "No corresponding transducer") . lookup agent . transducers 