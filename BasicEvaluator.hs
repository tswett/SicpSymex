module BasicEvaluator where

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

import qualified Data.Map as Map

import BasicEvaluatorTypes (
    Atom(Atom), Binding(Binding), bindingName,
    bindingValue, bindingZip, Closure(Closure),
    closureEnvironment, closureParameters, display,
    Environment(Environment), environmentAppend, environmentHead,
    environmentTail, fromSymex, nullEnvironmentP, parse,
    RecClosure(RecClosure), RecClosureDef(RecClosureDef), toSymex)
import Builtins (
    and_, append, applyBuiltin, atomP, builtinNames, cons, dataAtomP, eqP,
    head_, if_, listP, not_, nullP, second, tail_, third, zip_)
import Symex (Symex(SAtom, SList))

eval :: Symex -> Symex
eval expr = evalIn defaultEnvironment expr

evalIn :: Environment -> Symex -> Symex
evalIn env expr =
    if selfEvaluatingP expr then
        expr
    else if variableP expr then
        lookupVariableValue (fromSymex expr) env
    else if quotedP expr then
        textOfQuotation expr
    else if withP expr then
        evalIn (withEnvironment env expr) (withBody expr)
    else if lambdaP expr then
        toSymex $ makeClosure (lambdaParameters expr) (lambdaBody expr) env
    else if recfunP expr then
        toSymex $ makeRecClosure (recfunName expr) (recfunBody expr) env
    else if applicationP expr then
        apply (evalIn env (operator expr)) (evalEachIn env (operands expr))
    else
        error "eval: couldn't recognize this expression"

apply :: Symex -> [Symex] -> Symex
apply func args =
    if primitiveProcedureP func then
        applyPrimitiveProcedure func args
    else if closureP func then
        evalIn (extendEnvironment (closureParameters $ fromSymex func)
                                  args
                                  (closureEnvironment $ fromSymex func))
               (closureBody func)
    else if recClosureP func then
        apply (expandRecClosure func) args
    else
        error "apply: couldn't recognize this function"

rep :: String -> String
rep = display . eval . parse

repIn :: Environment -> String -> String
repIn env = display . evalIn env . parse

defaultEnvironment :: Environment
defaultEnvironment = Environment (map makePrimitive (fromSymex builtinNames))

map_ :: (Symex -> Symex) -> Symex -> Symex
map_ func list =
    if_ (nullP list)
        (SList [])
        (cons (func (head_ list)) (map_ func (tail_ list)))

makePrimitive :: Atom -> Binding
makePrimitive name = Binding name (SList [SAtom ":primitive", toSymex name])

selfEvaluatingP :: Symex -> Bool
selfEvaluatingP = fromSymex . dataAtomP

variableP :: Symex -> Bool
variableP = fromSymex . atomP

lookupVariableValue :: Atom -> Environment -> Symex
lookupVariableValue name env =
    if nullEnvironmentP env then
        error "lookup-variable-value: couldn't find the variable"
    else
        let binding = environmentHead env in
           if bindingName binding == name then
               bindingValue binding
           else
               lookupVariableValue name (environmentTail env)

quotedP :: Symex -> Bool
quotedP expr = startsWithP (SAtom "quote") expr

textOfQuotation :: Symex -> Symex
textOfQuotation = second

withP :: Symex -> Bool
withP expr = startsWithP (SAtom "with") expr

withEnvironment :: Environment -> Symex -> Environment
withEnvironment env expr =
    environmentAppend (fromSymex $ evalIn env (second expr)) env

withBody :: Symex -> Symex
withBody = third

lambdaP :: Symex -> Bool
lambdaP expr = startsWithP (SAtom "lambda") expr

makeClosure :: [Atom] -> Symex -> Environment -> Closure
makeClosure params body env = Closure params body env

lambdaParameters :: Symex -> [Atom]
lambdaParameters = fromSymex . second

lambdaBody :: Symex -> Symex
lambdaBody = third

recfunName :: Symex -> Atom
recfunName = fromSymex . second

recfunBody :: Symex -> [Symex]
recfunBody = tail . tail . fromSymex

makeRecClosure :: Atom -> [Symex] -> Environment -> RecClosure
makeRecClosure name defs env = RecClosure name (makeFunctionList defs) env

recfunP :: Symex -> Bool
recfunP expr = startsWithP (SAtom "recfun") expr

makeFunctionList :: [Symex] -> [RecClosureDef]
makeFunctionList defs = map makeFunction defs

makeFunction :: Symex -> RecClosureDef
makeFunction def = RecClosureDef (functionName def) (functionArgs def) (functionBody def)

functionName :: Symex -> Atom
functionName expr = fromSymex $ head_ (head_ expr)

functionArgs :: Symex -> [Atom]
functionArgs expr = tail (fromSymex $ head_ expr)

functionBody :: Symex -> Symex
functionBody expr = head_ (tail_ expr)

applicationP :: Symex -> Bool
applicationP expr = fromSymex (listP expr) && not (fromSymex (nullP expr))

operator :: Symex -> Symex
operator = head_

operands :: Symex -> [Symex]
operands = tail . fromSymex

evalEachIn :: Environment -> [Symex] -> [Symex]
evalEachIn env exprs =
    if null exprs then
        []
    else
        evalIn env (head exprs) : evalEachIn env (tail exprs)

primitiveProcedureP :: Symex -> Bool
primitiveProcedureP func = startsWithP (SAtom ":primitive") func

applyPrimitiveProcedure :: Symex -> [Symex] -> Symex
applyPrimitiveProcedure func args = applyBuiltin (head_ (tail_ func)) (toSymex args)

closureP :: Symex -> Bool
closureP func = startsWithP (SAtom ":closure") func

extendEnvironment :: [Atom] -> [Symex] -> Environment -> Environment
extendEnvironment newNames newValues oldEnvironment =
    environmentAppend (Environment $ bindingZip newNames newValues) oldEnvironment

closureBody :: Symex -> Symex
closureBody func = third func

recClosureP :: Symex -> Bool
recClosureP func = startsWithP (SAtom ":recclosure") func

expandRecClosure :: Symex -> Symex
expandRecClosure rc = SList [SAtom ":closure",
                             recClosureArgs rc,
                             recClosureBody rc,
                             append [recClosureBindings rc, recClosureEnv rc]]

recClosureName :: Symex -> Symex
recClosureName = second

recClosureArgs :: Symex -> Symex
recClosureArgs rc = recDefArgs $ lookupRecDef (recClosureName rc) (recDefs rc)

recClosureBody :: Symex -> Symex
recClosureBody rc = third (lookupRecDef (recClosureName rc) (recDefs rc))

recClosureBindings :: Symex -> Symex
recClosureBindings rc = map_ (makeRecClosureBinding rc) (recDefs rc)

recClosureEnv :: Symex -> Symex
recClosureEnv = third . tail_

recDefArgs :: Symex -> Symex
recDefArgs = second

lookupRecDef :: Symex -> Symex -> Symex
lookupRecDef name defs = if_ (eqP (head_ (head_ defs)) name)
                             (head_ defs)
                             (lookupRecDef name (tail_ defs))

makeRecClosureBinding :: Symex -> Symex -> Symex
makeRecClosureBinding rc recDef = SList [head_ recDef,
                                         SList [SAtom ":recclosure",
                                                head_ recDef,
                                                third rc,
                                                third (tail_ rc)]]

recDefs :: Symex -> Symex
recDefs = third

startsWithP :: Symex -> Symex -> Bool
startsWithP tag x = fromSymex (listP x) && head_ x == tag