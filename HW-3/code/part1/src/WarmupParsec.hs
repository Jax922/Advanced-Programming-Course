module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   <<< fill in here, if different from WarmupReadP >>>

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

-- Optional: if not attempted, leave as undefined
parseString :: String -> Either ParseError Exp
parseString = undefined
