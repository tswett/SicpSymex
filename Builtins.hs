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
import qualified Data.Map as Map

import Symex

applyBuiltin :: Symex -> Symex -> Symex
applyBuiltin (SAtom name) (SList arguments) = (builtinMap Map.! name) arguments
applyBuiltin (SList _) _ = error "apply-builtin: operator isn't an atom"
applyBuiltin _ (SAtom _) = error "apply-builtin: argument list isn't a list"

builtinMap :: Map.Map String ([Symex] -> Symex)
builtinMap =
    Map.fromList [
        ("atom?", wrap1 atomP),
        ("number?", wrap1 numberP),
        ("data-atom?", wrap1 dataAtomP),
        ("null?", wrap1 nullP),
        ("list?", wrap1 listP),
        ("if", wrap3 if_),
        ("and", wrap2 and_),
        ("or", wrap2 or_),
        ("not", wrap1 not_),
        ("eq?", wrap2 eqP),
        ("list", SList),
        ("cons", wrap2 cons),
        ("head", wrap1 head_),
        ("tail", wrap1 tail_)
    ]

wrap1 :: (Symex -> Symex) -> [Symex] -> Symex
wrap1 f [arg1] = f arg1
wrap1 _ _ = error "wrong number of arguments"

wrap2 :: (Symex -> Symex -> Symex) -> [Symex] -> Symex
wrap2 f [arg1, arg2] = f arg1 arg2
wrap2 _ _ = error "wrong number of arguments"

wrap3 :: (Symex -> Symex -> Symex -> Symex) -> [Symex] -> Symex
wrap3 f [arg1, arg2, arg3] = f arg1 arg2 arg3
wrap3 _ _ = error "wrong number of arguments"

atomP :: Symex -> Symex
atomP (SAtom _) = true
atomP _ = false

numberP :: Symex -> Symex
numberP (SAtom str) = boolToSymex (all isNumber str)
numberP _ = false

dataAtomP :: Symex -> Symex
dataAtomP (SAtom (':':_)) = true
dataAtomP _ = false

nullP :: Symex -> Symex
nullP (SList []) = true
nullP _ = false

listP :: Symex -> Symex
listP (SList _) = true
listP _ = false

if_ :: Symex -> Symex -> Symex -> Symex
if_ condition ifTrue ifFalse = if condition == true then ifTrue else ifFalse

and_ :: Symex -> Symex -> Symex
and_ x y = if_ x y x

or_ :: Symex -> Symex -> Symex
or_ x y = if_ x x y

not_ :: Symex -> Symex
not_ x = if_ x false true

eqP :: Symex -> Symex -> Symex
eqP x y = boolToSymex (x == y)

cons :: Symex -> Symex -> Symex
cons h (SList t) = SList (h:t)
cons _ _ = error "cons: tail isn't a list"

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