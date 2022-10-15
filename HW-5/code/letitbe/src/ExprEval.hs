{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)

type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) _ = return n
eval (Oper op x y) env = (oper op) <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env

evalTop e = eval e M.empty

simplify e =
  case e of
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Plus (Const 0) (Const c2) -> Const c2
    Oper Plus (Const c1) (Const 0) -> Const c1
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2)
    Oper Minus (Const 0) (Const c2) -> Const (-c2)
    Oper Minus (Const c1) (Const 0) -> Const c1
    Oper Times (Const 0) (Const _) -> Const 0
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)
    Oper Times (Const 1) (Const c2) -> Const c2
    Oper Times (Const c1) (Const 1) -> Const c1
    Oper Times (Const 0) (Const _) -> Const 0
    Oper Times (Const _) (Const 0) -> Const 0
    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body ->
      if isUsed v body then Let v (simplify e) (simplify body) else simplify body
    _ -> e

varIsUsed :: Ident -> Expr -> Bool
varIsUsed v e =
  case e of
    Const _ -> False
    Var s -> v == s
    Oper _ ex ex' -> varIsUsed v ex || varIsUsed v ex'
    Let s ex ex' -> v == s || varIsUsed v ex || varIsUsed v ex'

isUsed:: Ident -> Expr -> Bool
isUsed v body = case body of
  Var x -> if x == v then True else False
  Const _ -> False
  Oper _ x y -> (isUsed v x) || (isUsed v y)
  Let v' e' body -> (v == v') || (isUsed v e') || (isUsed v body)




