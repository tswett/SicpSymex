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
    and_, applyBuiltin, atomP, builtinNames, cons, dataAtomP, eqP,
    head_, if_, listP, not_, nullP, second, tail_)
import qualified Data.Map as Map
import Symex (Symex(SAtom, SList))

eval :: Symex -> Symex
eval expr = evalIn defaultEnvironment expr

evalIn :: Symex -> Symex -> Symex
evalIn env expr =
    if_ (selfEvaluatingP expr)
        expr .
    if_ (variableP expr)
        (lookupVariableValue expr env) .
    if_ (quotedP expr)
        (textOfQuotation expr) .
    if_ (applicationP expr)
        (apply (evalIn env (operator expr)) (evalEachIn env (operands expr))) $
    error "eval: couldn't recognize this expression"

apply :: Symex -> Symex -> Symex
apply func args =
    if_ (primitiveProcedureP func)
        (applyPrimitiveProcedure func args) $
    error "apply: couldn't recognize this function"

defaultEnvironment :: Symex
defaultEnvironment = map_ makePrimitive builtinNames

map_ :: (Symex -> Symex) -> Symex -> Symex
map_ func list =
    if_ (nullP list)
        (SList [])
        (cons (func (head_ list)) (map_ func (tail_ list)))

makePrimitive :: Symex -> Symex
makePrimitive name = SList [name, SList [SAtom ":primitive", name]]

selfEvaluatingP :: Symex -> Symex
selfEvaluatingP = dataAtomP

variableP :: Symex -> Symex
variableP = atomP

lookupVariableValue :: Symex -> Symex -> Symex
lookupVariableValue name env =
    if_ (nullP env)
        (error "lookup-variable-value: couldn't find the variable")
        (let assignment = head_ env in
            if_ (eqP (head_ assignment) name)
                (second assignment)
                (lookupVariableValue name (tail_ env)))

quotedP :: Symex -> Symex
quotedP expr = and_ (listP expr) (eqP (head_ expr) (SAtom "quote"))

textOfQuotation :: Symex -> Symex
textOfQuotation = second

applicationP :: Symex -> Symex
applicationP expr = and_ (listP expr) (not_ (nullP expr))

operator :: Symex -> Symex
operator = head_

operands :: Symex -> Symex
operands = tail_

evalEachIn :: Symex -> Symex -> Symex
evalEachIn env exprs =
    if_ (nullP exprs)
        (SList [])
        (cons (evalIn env (head_ exprs)) (evalEachIn env (tail_ exprs)))

primitiveProcedureP :: Symex -> Symex
primitiveProcedureP func = and_ (listP func) (eqP (head_ func) (SAtom ":primitive"))

applyPrimitiveProcedure :: Symex -> Symex -> Symex
applyPrimitiveProcedure func args = applyBuiltin (head_ (tail_ func)) args