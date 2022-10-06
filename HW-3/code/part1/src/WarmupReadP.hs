module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   <<< fill in here >>>
-- E ::= TE' | "-" TE'
-- E' ::= "+" TE' | "-" TE' | ε
-- T ::= num | "(" E ")

-- E ::= T Eopt 
-- Eopt  ::= num | "(" E ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

-- E ::= TE' | "-" TE'
expParse :: ReadP Exp
expParse = do
  expLeft <|> neg

--TE'
expLeft :: ReadP Exp
expLeft = do
  skipSpaces
  t <- tParse
  expOpt t

-- "-" T E'
neg :: ReadP Exp
neg = do
  skipSpaces
  string "-"
  t <- tParse 
  expOpt (Negate t)

-- T ::= num | "(" E ")"
tParse :: ReadP Exp
tParse = do 
  numParse <|> paren

--  E' ::= "+" TE' | "-" TE' | ε
expOpt :: Exp -> ReadP Exp
expOpt e = do
    skipSpaces
    op <- string "+" <|> string "-"
    t <- tParse
    expOpt $ createOpt op e t
    <|> return e
-- 
createOpt :: String -> Exp -> Exp -> Exp
createOpt s e1 e2
  | s == "+" = Add e1 e2
  | s == "-" = Add e1 (Negate e2)
  | otherwise = Add e1 e2 -- here just fix onlineTA hint

-- num 
numParse :: ReadP Exp
numParse = do
  skipSpaces
  n <- many1 $ satisfy isDigit  
  return (Num (read n))

-- "(" exp ")"
paren :: ReadP Exp
paren = do
  skipSpaces
  string "("
  inside <- expParse
  string ")"
  return inside

parseString :: String -> Either ParseError Exp
parseString s = 
  let res = readP_to_S expParse (filterSpaces s)
    in
      if null res || snd (last res) /= "" 
        then Left "ParseError"
      else Right (fst (last res))

-- filter spaces in input string
filterSpaces :: String -> String
filterSpaces s = 
  let s'' = filter (/='\t') s'
    in filter (/='\n') s''
  where s' = filter ( /=' ') s

