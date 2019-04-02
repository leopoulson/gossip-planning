module Drinkers where

import Model

type BoolT = (Bool, Bool, Bool)

bools = [True, False]

initBar :: EpistM (Bool, Bool, Bool)
initBar = Mo states [a, b, c] [] rels [(True, True, True)]
  where
    states = [(b1, b2, b3) | b1 <- bools, b2 <- bools, b3 <- bools]
    rela = (a, [[(True, x, y)  | x <- bools, y <- bools],
                [(False, x, y) | x <- bools, y <- bools]])
    relb = (b, [[(x, True, y)  | x <- bools, y <- bools],
                [(x, False, y) | x <- bools, y <- bools]])
    relc = (c, [[(x, y, True)  | x <- bools, y <- bools],
                [(x, y, False) | x <- bools, y <- bools]])
    rels = [rela, relb, relc]
