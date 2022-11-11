-- Put your Coder implementation in this file
module CoderImpl where

import Defs
import Control.Monad (ap, liftM)

-- no need to touch these
instance Functor Tree where fmap = liftM
instance Applicative Tree where pure = return; (<*>) = ap

instance Monad Tree where
  return = undefined
  (>>=) = undefined

pick :: [a] -> Tree a
pick = undefined

solutions :: Tree a -> Int -> Maybe a -> [a]
solutions = undefined

produce :: [(String,SType)] -> SType -> Tree Exp
produce = undefined

-- recommended, but not mandated, helper function:
extract :: [(String,SType)] -> SType -> SType -> Tree (Exp -> Exp)
extract = undefined
