-- Put your Parser implementation in this file
module ParserImpl where

import Defs
-- import either ReadP or Parsec, as relevant
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))

type Parser a = ReadP a

parseStringType :: String -> EM PType
parseStringType = parseTop pType

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz = parseTop pTDeclz

-- generic ReadP top-level parser
parseTop :: Parser a -> String -> EM a
parseTop p s =
    case readP_to_S (between skipws eof p) s of
        [] -> Left "no parse"
        [(t, _)] -> return t
        _ -> error "ambiguous grammar"

-- skip all whitespace and comments
skipws :: Parser ()
skipws = do skipSpaces; optional (do string "{-"; endComment; skipws)

-- skip remainder of comment
endComment :: Parser ()
endComment = do string "-}"; return ()
                <++ do get; endComment  -- biased to end comment if possible

-- skip ws after token parser
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipws; return a

-- expect specific symbolic token
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- word constituents: letters, digits, underscores, primes
-- (not particularly efficient, but input strings are usually small)
wordChars :: [Char]
wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','\'']

-- parse a single word
pWord :: Parser String
pWord = lexeme $ munch1 (`elem` wordChars)

-- expect specific keyword
keyword :: String -> Parser ()
keyword s = do s' <- pWord; if s' == s then return () else pfail

reserved = ["type", "newtype", "data"]

-- variable name: word starting with lowercase letter and not reserved
pVName :: Parser String
pVName = do s <- pWord
            if head s `elem` ['a'..'z'] && s `notElem` reserved then return s
            else pfail

-- constructor name: word starting with uppercase letter (never reserved)
pCName :: Parser String
pCName = do s <- pWord
            if head s `elem` ['A'..'Z'] then return s else pfail

-- 
-- Type ::= vName
-- Type ::= Type '->' Type
-- Type ::= '(' Type ',' Type ')'
-- Type ::= cName Typez
-- Type ::= '(' Type ')'
-- Typez ::=  ^ | Type Typez

--
-- Type  ::= RType | RType '->' Type
-- RType ::= BType | cName { AType }
-- AType ::= BType | cName
-- BType ::= vName | '(' Type [',' Type ] ')'
pType, pRType, pAType, pBType :: Parser PType

pType = pRType `chainr1`
            (do symbol "->"; return $ \a1 a2 -> PTApp "(->)" [a1, a2])

pRType = pBType
            <|> do r <- pCName; ts <- many pAType; return $ PTApp r ts

pAType = pBType
            <|> do r <- pCName; return $ PTApp r []

pBType = do b <- pVName; return $ PTVar b
         <|> (between (symbol "(") (symbol ")") $
              do t1 <- pType
                 option t1 (do symbol ","; t2 <- pType
                               return $ PTApp "(,)" [t1, t2]))

-- non-left-recursive grammar
-- TDeclz ::= eps | ';' TDeclz | TDecl TDeclz
pTDeclz :: Parser [TDecl]
pTDeclz = do return []
          <|> do symbol ";"; pTDeclz
          <|> do d <- pTDecl; ds <- pTDeclz; return (d : ds)

pTDecl :: Parser TDecl
pTDecl = 
  choice [  do keyword "type"; td <- pTDHead; symbol "="; p <- pType
               return $ TDSyn td p,
            do keyword "newtype"; td <- pTDHead; symbol "="; c <- pCName
               symbol "{"; v <- pVName; symbol "::"; p <- pType; symbol "}"
               return $ TDRcd td c [(v, p)],
            do keyword "data"; td <- pTDHead; symbol "="; c <- pCName
               symbol "{"; fd <- pFDeclz; symbol "}"
               return $ TDRcd td c fd ]

pTDHead :: Parser TDHead
pTDHead = do c <- pCName; as <- many pVName
             return (c, as)

pFDeclz :: Parser [(FName, PType)]
pFDeclz = do fs <- pVName `sepBy1` symbol ","; symbol "::"; t <- pType
             return [(f,t) | f <- fs] --all fields share same type

