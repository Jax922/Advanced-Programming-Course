module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified Data.Map.Strict as M
-- import Data.String (String)
import Data.List (isInfixOf)
-- import ExprAst (Expr)
-- import Control.Applicative (Const(Const))
-- import Control.Monad (Functor(fmap))


instance Arbitrary Expr where
   arbitrary = sized exprN

   shrink (Const n) = map Const $ shrink n
   shrink (Var i) = map Var $ shrink i
   shrink(Oper op x y) = [x, y] ++ [Oper op x' y' | (x', y') <- shrink(x, y)]
   shrink(Let i x y) = [x, y] ++ [Let i x' y' | (x', y') <- shrink(x, y)]

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = collect (x) $ E.eval(E.simplify x) M.empty === E.eval x M.empty

arbitraryOp :: Gen Op
arbitraryOp = elements [Plus, Minus, Times]

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

arbitraryIdent :: Gen String
arbitraryIdent = listOf1 genSafeChar

-- helper function
-- for generate the `Let` expressions do not contain `Var` key word
isVarInLet :: Expr -> Bool
isVarInLet e = not ( "Var" `isInfixOf` show e )

exprN :: Int -> Gen Expr
exprN 0 =
      fmap Const arbitrary
exprN n = oneof
   [
      fmap Const arbitrary,
      fmap Var arbitraryIdent,
      do
         x <- exprN(n `div` 2)
         y <- exprN(n `div` 2)
         o <- arbitraryOp
         return $ Oper o x y, 
      do
         i <- arbitraryIdent
         x <- suchThat (exprN(n `div` 2)) isVarInLet -- not contain `Var`
         y <- exprN(n `div` 2)
         return $ Let i x y
   ]


