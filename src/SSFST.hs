module SSFST where

type SSTransition ch = ch -> [ch]

data SSFST ch = SSFST {
    alphabet :: [ch],                      -- Alphabet
    ssTransition :: SSTransition ch        -- Transition
}

