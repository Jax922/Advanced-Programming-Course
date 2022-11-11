-- Put yor parser implementation in this file
module ParserImpl where

import Definitions
-- import ReadP or Parsec
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))
import qualified Data.Char as Data

-- import Data.Char
import Data.Char (isLetter, isPrint, isLower)
import Data.List (intercalate, last, find)
import Text.Read (Lexeme(String))

type Parser a = ReadP a

parseSpec :: String -> EM (String, EGrammar)
parseSpec s =
  case readP_to_S pGrammar s of
    [] -> Left "no parse"
    [(t, _)] -> return t
    _ -> error "ambiguous grammar"

pGrammar :: Parser (String, EGrammar)
pGrammar = do p <- between skipSpaces (symbol "---") pPreamble
              rs <- between skipws eof pERules; return(p, rs)

-- skip all whitespace and comments
skipws :: Parser ()
skipws = do skipSpaces; optional (do string "--"; endComment; skipws)

endComment :: Parser ()
endComment = do string "\n"; return ()
                <++ do get; endComment

-- skip ws after token parser
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipws; return a

-- expect specific symbolic token
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

symbolChar :: Char -> Parser ()
symbolChar c = lexeme $ do char c; return ()

-- word constituents: letters, digits, underscores,
-- (not particularly efficient, but input strings are usually small)
wordChars :: [Char]
wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

-- parse a single word
pWord :: Parser String
pWord = lexeme $ munch1 (`elem` wordChars)

--  Any sequence of (Unicode) letters, digits, and underscores,
--  starting with a letter. There are no reserved names.
pName :: Parser String
pName = do s <- pWord
           if isLetter $ head s then return s else pfail

pTokLit :: Parser Simple
pTokLit = do
  symbol "\""
  ts <- pToken `sepBy` string "\"\""
  symbol "\""
  let token = intercalate "\"" ts
  if token == "" then pfail -- when toekn == ""
  else
    case find (=='"') token of
      Just _ -> pfail -- contain a double-quote, it must be written as ""
      Nothing -> return $ SLit token

pToken :: Parser String
pToken = do munch $ (\c -> isPrint c && c /= '"')

pAnyChar :: Parser Simple
pAnyChar = do symbol "@"; return SAnyChar

pCharLit :: Parser Simple
pCharLit = do symbol "'"; c <- satisfy isPrint; symbol "'";
              return $ SChar c

pHtext :: Parser String
pHtext =
  -- do pBracketsHtext
  -- <++
  do munch (/='}')
  -- do
  --   -- s <- look
  --   -- many get
  --   -- let s = init l
  --   -- many get
  --   s' <- munch (\c -> c /=' ' || c /= '.' || c /= '\n')
  --   return s'
    -- let s = init s'
    -- -- return l
    -- -- let safeHead :: String -> EM Char
    -- --     safeHead "" = Left "The string was empty."
    -- --     safeHead xs = Right . head $ xs
    -- let safeTail :: String -> EM String
    --     safeTail "" = Left "The string was empty."
    --     safeTail xs = Right . tail $ xs
    -- let f :: String -> EM String
    --     f "" = Right ""
    --     f (c:cs) =  case c of
    --                   '{' ->
    --                     case safeHead cs of
    --                       Left e -> Left e
    --                       Right c' -> if c' == '{' then fmap ("{" ++) (f $ tail cs) else Left "left brace error"
    --                   '}' ->
    --                     case safeHead cs of
    --                       Left e -> Left e
    --                       Right c' -> if c' == '}' then fmap ("}" ++) (f $ tail cs) else Left "right brace error"
    --                   _ ->  case mc' of
    --                           Left e -> Left e
    --                           Right c' -> Right (c : c')
    --                         where mc' = f cs
    -- case f s of
    --   Left _ -> pfail
    --   Right s'' ->
    --     case safeTail s'' of
    --       Left _ -> return s''
    --       Right s''' ->
    --         case safeHead s''' of
    --           Left _ -> return s''
    --           Right c'' ->
    --             case c'' of
    --               ':' -> pfail
    --               '?' -> pfail
    --               _ -> return s''
-- ??
-- @todo return error with pfail
pBracketsHtext :: Parser String
pBracketsHtext =
  between (string "{{") (string "}}") $
   do
      many $ satisfy (\c -> c /= ':' || c /= '?')

-- Atom ::= name | tokLit | "@" | charLit | "(" Alts ")".
pAtom :: Parser Simple
pAtom = do SNTerm <$> pName <|> pTokLit <|> pAnyChar <|> pCharLit

-- Simple0 ::= Atom | Simple0 "{?" htext "}".
--
-- Simple0  ::= Simple0' | "{?" htext "}" Simple0'
-- Simple0' ::= Atom
pSimple0 :: Parser Simple
pSimple0 =
  do pSimple0'
  <|>
  do symbol "{?"; h <- pHtext; s0' <- pSimple0'
     return $ SPred s0' h

pSimple0' :: Parser Simple
pSimple0' = pAtom

-- Simple ::= Simple0 | Simple0 "?" | Simple0 "*" | "!" Simple0.
pSimple :: Parser Simple
pSimple =
  choice [
    pSimple0,
    do s <- pSimple0; symbol "?"; return $ SPred s "?",
    do s <- pSimple0; symbol "*"; return $ SPred s "*",
    do symbol "!"; SNot <$> pSimple0;
  ]

-- Simplez ::= | Simple Simplez.
pSimplez :: Parser [Simple]
pSimplez = many pSimple

-- Seq ::= Simple | Simplez "{" htext "}" | "(" Alts ")".
pSeq :: Parser ERHS
pSeq = do ESimple <$> pSimple;
       <|>
       do ss <- pSimplez; symbol "{"; h <- pHtext; symbol "}"
          return $ ESeq [simpleToERHS s | s <- ss] h

-- @todo delete
simpleToERHS :: Simple -> ERHS
simpleToERHS (SNot s) = ENot $ ESimple s
simpleToERHS (SPred s ht) =
  case ht of
    "?" -> EOption $ ESimple s -- EBNF notation “[···]”
    "*" -> EMany $ ESimple s -- EBNF notation “{···}”
    _ -> EPred (ESimple s) ht
simpleToERHS s = ESimple s

-- Alts ::= Seq | Seq "|" Alts | "(" Alts ")" | Seq "(" Alts ")".
pAlts :: Parser ERHS
pAlts = do pSeq
        <|>
        do pNestedAlts
        <|>
        do seq <- pSeq; symbol "|"; EBar seq <$> pAlts

pNestedAlts :: Parser ERHS
pNestedAlts = 
  do symbol "("; a <- pAlts; symbol ")"; return a
  <|>
  do s <- pSeq; 

-- OptType ::= | "{:" htext "}".
pOptType :: Parser Type
pOptType = do symbol "{:"; h <- pHtext; symbol "}"; return $ AUser h

-- LHS ::= name OptType | "_".
pLHS :: Parser RLHS
pLHS =
  do n <- pName; o <- pOptType; return (n, transformRKind n, Just o)
  <|>
  do n <- pName; return (n, transformRKind n, Nothing)
  <|>
  do symbol "_"; return ("_", RSep, Nothing)

transformRKind :: String -> RKind
transformRKind s = 
  case safeHead s of
    Left _ -> error "this string is empty"
    Right c -> if isLower c then RToken else RPlain

-- ERule ::= LHS "::=" Alts ".".
pERule :: Parser ERule
pERule = do l <- pLHS; symbol "::="; r <- pAlts; symbol "."; return (l, r)

-- ERules ::= ERule | ERule ERules.
pERules :: Parser [ERule]
pERules = many1 pERule

-- Spec ::= preamble ERules.
pPreamble :: Parser String
pPreamble = do many get;

-- helper uilt funcitons
safeHead [] = Left "The list was empty"
safeHead xs = Right . head $ xs