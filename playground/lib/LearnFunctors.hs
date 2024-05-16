{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module LearnFunctors where

newtype Identity a = Identity a

data Pair a = Pair a a

data Two a b = Two a b

data Three a b c = Three a b c

data Three' a b = Three' a b b

data Four a b c d = Four a b c d

data Four' a b = Four' a a a b
