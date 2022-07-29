module Symex where

import Data.Char (isLetter, isNumber, isSpace)

data Symex = SAtom String | SList [Symex] deriving (Eq, Show)

isAtomChar :: Char -> Bool
isAtomChar c = isLetter c || isNumber c || c `elem` "_-:?"

parse :: String -> Symex
parse str = fst (parse' str)

parse' :: String -> (Symex, String)
parse' "" = error "parse': unexpected end of input"
parse' ('(' : remainder1) = let (list, remainder2) = parseList' remainder1
                            in (SList list, remainder2)
parse' (x : remainder) | isSpace x = parse' remainder
parse' str | isAtomChar (head str) = (SAtom (takeWhile isAtomChar str), dropWhile isAtomChar str)

parseList' :: String -> ([Symex], String)
parseList' (')': remainder) = ([], remainder)
parseList' (x : remainder) | isSpace x = parseList' remainder
parseList' str = let (hd, remainder1) = parse' str
                     (tl, remainder2) = parseList' remainder1
                 in (hd : tl, remainder2)

display :: Symex -> String
display (SAtom str) = str
display (SList list) = "(" ++ unwords (map display list) ++ ")"