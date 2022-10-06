-- Do not modify this file!

module Definitions where

data Exp =
-- Simple
    Cst Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
-- Full
  | If {test, yes, no :: Exp}
  | Var VName
  | Let {var :: VName, def, body :: Exp}
  | Sum {var :: VName, from, to, body :: Exp}
  deriving (Eq, Show)

type VName = String

type Env = VName -> Maybe Integer

initEnv :: Env
initEnv = \_v -> Nothing

data ArithError =
    EBadVar VName  -- undefined variable
  | EDivZero       -- attempted division by zero
  | ENegPower      -- attempted raising to negative power
  | EOther String  -- any other errors, if relevant
  deriving (Eq, Show)
