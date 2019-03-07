module RS where

import FSM 
import FST
import Model

-- Definition for transducers is a bit unsatisfactory
data RegularStructure ch st = RegularStructure {
    dAutomata :: FSM ch st, 
    transducers :: [(Agent, FST ch ((st, st), st))], 
    fAutomata :: Prop -> FSM ch st
}