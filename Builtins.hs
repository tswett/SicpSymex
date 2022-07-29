module Builtins where

-- Copyright 2022 by Tanner Swett.
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of version 3 of the GNU General Public License as published by the
-- Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for
-- more details.

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