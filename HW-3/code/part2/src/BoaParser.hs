-- Skeleton file for Boa Parser.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module BoaParser (ParseError, parseString, forClauseP, ifClauseP) where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import Data.List
import BoaAST
-- add any other other imports you need

newtype ParseError
    = ParseError String
    deriving (Eq, Show)
-- instance here

--------- program parse 
program :: ReadP Program
program = 
    trim (
        do
         stmts
    )

-- ----------------------------stmts\stmt parse
stmts :: ReadP [Stmt]
stmts =
    do
        st <- stmt
        skipSpaces
        string ";"
        skipSpaces
        sts <- stmts
        skipSpaces
        return (st:sts)
    <|>
    do
        st <- stmt
        skipSpaces
        return [st]

stmt :: ReadP Stmt
stmt =
    do
        skipSpaces
        i <- ident
        skipSpaces
        string "="
        skipSpaces
        e <- expr
        skipSpaces
        return (SDef i e)
    <|>
    do
        skipSpaces
        e <- expr
        skipSpaces
        return (SExp e)
-- -----------------------------ident parse 
ident :: ReadP String
ident = trim (do
    fstC <- count 1 isNotDigit
    restC <- many $ satisfy (\c -> isAlpha c || c == '_' || isDigit c)
    -- optional skipComment
    if isReservedWords (fstC ++ restC)
        then pfail -- "can not be reserve words"
    else
        return (fstC ++ restC))

isNotDigit :: ReadP Char
isNotDigit =
    satisfy (\c -> isLetter c || c  == '_')

isReservedWords :: String -> Bool
isReservedWords s =
    s `elem` ["None", "True", "False", "for", "if", "in", "not"]

-- ---------------------------- numConst parse
numConst :: ReadP Exp
numConst = trim (
    oneNum <|> moreTwoNum
    )

oneNum :: ReadP Exp
oneNum = trim (do
    fstC <- count 1 isOneDigit
    skipSpaces
    optional skipComment
    return (Const (IntVal (read fstC))))

moreTwoNum :: ReadP Exp
moreTwoNum = trim (do
    fstC <- string "-" <|> string "+" <|> count 1 isOneDigit
    spaces <- munch isSpace
    endC <- munch1 isDigit
    skipSpaces
    if (fstC == "0" || fstC == "+") && endC /= "" || any isSpace spaces then pfail else
        return (Const (IntVal (read (fstC ++ endC)))))

isOneDigit :: ReadP Char
isOneDigit =
    satisfy isDigit

-- -------------------------- stringConst parse
stringConst :: ReadP Exp
stringConst = trim (do
    string "'"
    s <- many $ satisfy isAscii
    string "'"
    skipSpaces
    if illegalString s
        then pfail
    else
        return (Const (StringVal (convertString s))))

-- escape  = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+)
-- REFERENCE :http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html
illegalString :: String -> Bool
illegalString s =
    s /= "" && head s == '\\' || elem s ["\DEL", "\n", "\t", "\NUL", "'", "''"]
        || not (null (readP_to_S skipComment s))

-- helper function for stringConst
-- \\n -> \n, \\\\ -> \\, filter \\\n
convertString :: String -> String
convertString "" = ""
convertString (x:xs)
    | x == '\\' = case head xs of
                    'n' -> '\n' : convertString (tail xs)
                    '\n' -> convertString $ tail xs
                    '\\' -> '\\' : convertString (tail xs)
                    _ -> convertString xs
    | otherwise = x : convertString xs

-- -----------------------None\True\False\not parse
noneP :: ReadP Exp
noneP = trim (do
    string "None"
    skipSpaces
    return (Const NoneVal))

trueP :: ReadP Exp
trueP = trim (do
    string "True"
    skipSpaces
    return (Const TrueVal))

falseP :: ReadP Exp
falseP = trim (do
    string "False"
    skipSpaces
    return (Const FalseVal))

notP :: ReadP Exp
notP = trim (do
    string "not"
    skipSpaces
    e <- expr
    skipSpaces
    return (Not e))
-- ----------------------- ‘(’ Expr ‘)’ parse
nest :: ReadP Exp
nest = trim (do
    string "("
    skipSpaces
    e <- expr
    skipSpaces
    string ")"
    skipSpaces
    return e)

-- ----------------------- expr parse

-- Expr ::= "not" Expr | expr'
-- expr' ::= expr'' LExpr |  expr''
-- LExpr = LOper expr'' LExpr |  LOper expr'' LExpr 
-- CExper = COper 
-- LOper = ArithOper | FactOper
-- ArithOper = "+" | "-"
-- FactOper = "*" | "//" | "%" 
-- COper = "==" | "!=" | "<" | "<=" | ">" | ">=" | "in" | "not" "in" 
-- expr'' :: = "(" Expr ")" | ident "(" Exprz ")" | "[" Exprz "]" | "[" Expr ForClause Clausez "]" |  numConst | stringConst | "None" | "True" | "False" | ident

expr :: ReadP Exp
expr = trim (
        listExprz <++ ( notP <|> expr'))

-- expr' ::= expr'' LExpr |  expr'' COper expr'
expr' :: ReadP Exp
expr' = trim (
    do
        e'' <- expr''
        lExpr e''
    <|>
    do
        expr''

    )

-- LExpr = LOper expr'' LExpr
lExpr :: Exp -> ReadP Exp
lExpr e = trim (do
                    s <- lOp
                    e'' <- expr''
                    lExpr (Oper (getOp s) e e'')
                <|>
                do
                    many1 $ satisfy isSpace
                    cop <- string "in"
                    many1 $ satisfy isSpace
                    e'' <- expr''
                    optional skipComment
                    lExpr (getComplexOp cop e e'')
                <|>
                do
                    skipSpaces
                    cop <- cOP
                    skipSpaces
                    e'' <- expr''
                    skipSpaces
                    optional skipComment
                    op' <- look
                    if null op'
                        then do
                                lExpr (getComplexOp cop  e e'')
                    else
                        if  (head op' /= ',' ||  head op' /= ',')
                                && any (`isInfixOf` op') nonAssociative-- compare operations are nonAssociative
                            then
                                pfail
                        else
                            do
                                lExpr (getComplexOp cop  e e'')

                <|>
                 return e)

nonAssociative :: [String]
nonAssociative = [">", ">=", "<", "<=", "==", "!=", "not in", "not\tin", "in"]

-- expr'' :: = "(" Expr ")" | ident "(" Exprz ")" | "[" Exprz "]" | "[" Expr ForClause Clausez "]" |  numConst | stringConst | "None" | "True" | "False" | ident
expr'' :: ReadP Exp
expr'' = do
    listForClauseP <|> identExprz <|> nest <|> listExprz
    <|>numConst <|> varP <|> stringConst
        <|> noneP <|> trueP <|> falseP

-- ------------------ident (var parse)
varP :: ReadP Exp
varP = trim (do
    -- skipSpaces
    i <- ident
    optional skipComment
    return (Var i))

-- ------------------oper parse
getOp :: String -> Op
getOp s
 | s == "+" = Plus
 | s == "-" = Minus
 | s == "*" = Times
 | s == "//" = Div
 | s == "%" = Mod
 | s == "==" = Eq
 | s == "<" = Less
 | s == ">" = Greater
 | s == "in" = In

getComplexOp :: String -> Exp -> Exp -> Exp
getComplexOp s e1 e2
    | s == "!=" = Not (Oper Eq e1 e2)
    | s == "<=" = Not (Oper Greater e1 e2)
    | s == ">=" = Not (Oper Less e1 e2)
    | s == "not\tin" = Not (Oper In e1 e2)
    | s == "not in" = Not (Oper In e1 e2)
    | otherwise = Oper (getOp s) e1 e2

-- LOper = ArithOper | FactOper
lOp :: ReadP String
lOp =
    arithmeticOp <++ factOp

-- compare operation parse,such as >,<,==,!=,<=,>= ...
cOP ::ReadP String
cOP = do
    string ">" <|>  string "==" <|> string "<" <|> string "not in" <|> string "not\tin"
        <|> string "!=" <|> string ">=" <|> string "<="

-- ArithOper = "+" | "-"
arithmeticOp :: ReadP String
arithmeticOp =
    string "-" <|> string "+"

-- FactOper = "*" | "//" | "%" 
factOp :: ReadP String
factOp =
    string "*" <|> string "//" <|> string "%"

-- ----------- exprs\exprz parse
exprs :: ReadP [Exp]
exprs = trim (
    do
        e <- expr
        return [e]
    <|>
    do
        e<-expr
        skipSpaces
        string ","
        skipSpaces
        es<-exprs
        return (e:es))

exprz :: ReadP [Exp]
exprz =
    exprs <|> return []

-- ---------- ident ‘(’ Exprz ‘)’
identExprz :: ReadP Exp
identExprz = trim (do
    i <- ident
    skipSpaces
    string "("
    skipSpaces
    ez <- exprz
    skipSpaces
    string ")"
    skipSpaces
    return (Call i ez))

-- ---------- ‘[’ Exprz ‘]’
listExprz :: ReadP Exp
listExprz = trim (do
    string "["
    skipSpaces
    ez <- exprz
    skipSpaces
    string "]"
    return (List ez))

-- ---------- ‘[’ Expr ForClause Clausez ‘]’
listForClauseP :: ReadP Exp
listForClauseP = trim (do
    string "["
    skipSpaces
    e <- expr
    many1 $ satisfy isSpace
    skipSpaces
    f <- forClauseP
    skipSpaces
    c <- clausezP f
    skipSpaces
    string "]"
    return (Compr e c))

-- ---------- IfClause ::== ‘if’ Expr
ifClauseP :: ReadP CClause
ifClauseP = trim(do
    string "if"
    char ' '
    optional skipComment
    CCIf <$> expr)

-- ---------- ForClause ::== ‘for’ ident ‘in’ Expr
forClauseP :: ReadP CClause
forClauseP = do
    many $ satisfy isSpace
    string "for"
    many1 $ satisfy isSpace
    i <- ident
    many1 $ satisfy isSpace
    string "in"
    many1 $ satisfy isSpace
    skipSpaces
    e <- expr
    return (CCFor i e)

-- ---------- Clausez 
clausezP :: CClause -> ReadP [CClause]
clausezP c =
    do
        f <- forClauseP
        c' <- clausezP f
        return (c:c')
    <|>
    do
        i <- ifClauseP
        c' <- clausezP i
        return (c:c')
    <|>
    return [c]

parseString :: String -> Either ParseError Program
parseString s =
    let res = readP_to_S program s
    in
      if null res || snd (last res) /= ""
        then Left (ParseError "ParseError")
      else Right (fst (last res))

-- skip whitespaces and comment
trim :: ReadP a -> ReadP a
trim e =
    do
        optional skipComment
        e
    <++
    do
        skipSpaces
        e

-- 
skipComment:: ReadP String
skipComment =
    do
        skipSpaces
        string "#"
        cm <- many $ satisfy isAscii
        string "\n"
        return cm
    <++
    do
        skipSpaces
        string "#"
        many $ satisfy isAscii