module Basics where

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

import Builtins (
    and_, cons, dataAtomP, evalBuiltin, head_, if_, listP, not_, nullP, tail_)
import Symex (Symex(SList))

eval :: Symex -> Symex
eval expr = evalIn (SList []) expr 

evalIn :: Symex -> Symex -> Symex
evalIn env expr =
    if_ (selfEvaluatingP expr)
        expr .
    if_ (isApplicationP expr)
        (evalBuiltin (operator expr) (evalEachIn env (operands expr))) $
    error "eval: couldn't recognize this expression"

selfEvaluatingP :: Symex -> Symex
selfEvaluatingP = dataAtomP

isApplicationP :: Symex -> Symex
isApplicationP expr = and_ (listP expr) (not_ (nullP expr))

operator :: Symex -> Symex
operator = head_

operands :: Symex -> Symex
operands = tail_

evalEachIn :: Symex -> Symex -> Symex
evalEachIn env exprs =
    if_ (nullP exprs)
        (SList [])
        (cons (evalIn env (head_ exprs)) (evalEachIn env (tail_ exprs)))