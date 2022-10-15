-- Abstract syntax definitions for expressions. Do not modify anything!

module ExprAst where

data Expr = Const Int
          | Var Ident
          | Oper Op Expr Expr
          | Let Ident Expr Expr
          deriving (Eq, Show, Read)

type Ident = String

data Op = Plus | Minus | Times
        deriving (Eq, Show, Read)
