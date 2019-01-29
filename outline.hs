module Outline where

newtype Prop = P Int

type Agent = String

data LEp = Top | Prop | Not LEp | And LEp LEp | K Agent LEp