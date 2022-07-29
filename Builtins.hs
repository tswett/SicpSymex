module Builtins where

import Data.Char (isNumber)

import Symex

numberP :: Symex -> Symex
numberP (SAtom str) = boolToSymex (all isNumber str)
numberP _ = false

dataAtomP :: Symex -> Symex
dataAtomP (SAtom (':':_)) = true
dataAtomP _ = false

nullP :: Symex -> Symex
nullP (SList []) = true
nullP _ = false

head_ :: Symex -> Symex
head_ (SList (x:xs)) = x
head_ (SList []) = error "head: expected non-empty list, found empty list"
head_ (SAtom _) = error "head: expected non-empty list, found atom"

tail_ :: Symex -> Symex
tail_ (SList (x:xs)) = SList xs
tail_ (SList []) = error "tail: expected non-empty list, found empty list"
tail_ (SAtom _) = error "tail: expected non-empty list, found atom"

boolToSymex :: Bool -> Symex
boolToSymex True = true
boolToSymex False = false

true :: Symex
true = SAtom ":true"

false :: Symex
false = SAtom ":false"