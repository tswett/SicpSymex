module BasicEvaluatorTypes where

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

import Builtins (boolToSymex, symexToBool)
import Symex (Symex(SAtom, SList))
import qualified Symex

class SymexType a where
    toSymex :: a -> Symex
    fromSymex :: Symex -> a



parse :: SymexType a => String -> a
parse x = fromSymex (Symex.parse x)

display :: SymexType a => a -> String
display x = Symex.display (toSymex x)



instance SymexType Symex where
    toSymex = id
    fromSymex = id



instance SymexType Bool where
    toSymex = boolToSymex
    fromSymex = symexToBool



instance SymexType a => SymexType [a] where
    toSymex values = SList (map toSymex values)
    fromSymex (SList values) = map fromSymex values
    fromSymex (SAtom _) = error "[].fromSymex: expected list, found atom"



data Atom = Atom String deriving Eq

instance SymexType Atom where
    toSymex (Atom str) = SAtom str
    fromSymex (SAtom str) = Atom str
    fromSymex (SList _) = error "Atom.fromSymex: expected atom, found list"



data Binding = Binding Atom Symex

instance SymexType Binding where
    toSymex (Binding (Atom name) value) = SList [SAtom name, value]
    fromSymex (SList [SAtom name, value]) = Binding (Atom name) value
    fromSymex _ = error "Binding.fromSymex: ill-formed binding"

bindingName :: Binding -> Atom
bindingName (Binding name _) = name

bindingValue :: Binding -> Symex
bindingValue (Binding _ value) = value

bindingZip :: [Atom] -> [Symex] -> [Binding]
bindingZip (name : names) (value : values) = Binding name value : bindingZip names values
bindingZip (_:_) [] = error "bindingZip: too many names"
bindingZip [] (_:_) = error "bindingZip: too many values"
bindingZip [] [] = []



data Environment = Environment [Binding]

instance SymexType Environment where
    toSymex (Environment bindings) = SList (map toSymex bindings)
    fromSymex (SList bindings) = Environment (map fromSymex bindings)

nullEnvironmentP :: Environment -> Bool
nullEnvironmentP (Environment []) = True
nullEnvironmentP _ = False

environmentHead :: Environment -> Binding
environmentHead (Environment (b:_)) = b
environmentHead (Environment []) = error "environment-head: empty environment"

environmentTail :: Environment -> Environment
environmentTail (Environment (_:bs)) = Environment bs
environmentTail (Environment []) = error "environment-tail: empty environment"

environmentAppend :: Environment -> Environment -> Environment
environmentAppend (Environment bs1) (Environment bs2) = Environment (bs1 ++ bs2)



data Closure = Closure [Atom] Symex Environment

instance SymexType Closure where
    toSymex (Closure params body env) =
        SList [SAtom ":closure", toSymex params, body, toSymex env]
    fromSymex (SList [SAtom ":closure", params, body, env]) =
        Closure (fromSymex params) body (fromSymex env)
    fromSymex _ = error "Closure.fromSymex: ill-formed closure"

closureParameters :: Closure -> [Atom]
closureParameters (Closure params _ _) = params

closureEnvironment :: Closure -> Environment
closureEnvironment (Closure _ _ env) = env



data RecClosureDef = RecClosureDef Atom [Atom] Symex

instance SymexType RecClosureDef where
    toSymex (RecClosureDef name params body) =
        SList [toSymex name, toSymex params, body]
    fromSymex (SList [name, params, body]) =
        RecClosureDef (fromSymex name) (fromSymex params) body
    fromSymex _ =
        error "RecClosureDef.fromSymex: ill-formed recclosure definition"



data RecClosure = RecClosure Atom [RecClosureDef] Environment

instance SymexType RecClosure where
    toSymex (RecClosure name defs env) =
        SList [SAtom ":recclosure", toSymex name, toSymex defs, toSymex env]
    fromSymex (SList [SAtom ":recclosure", name, defs, env]) =
        RecClosure (fromSymex name) (fromSymex defs) (fromSymex env)
    fromSymex _ =
        error "RecClosure.fromSymex: ill-formed recclosure"