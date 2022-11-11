-- page 315 《Haskell函数式编程》
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Exp = Lit Integer
          | Add Exp Exp
          | Sub Exp Exp
          | Mul Exp Exp
          | Div Exp Exp

-- safeEval :: Exp -> Maybe Integer
-- safeEval (Lit n) = Just n
-- safeEval (Add e1 e2) =
--     case safeEval e1 of
--     Nothing -> Nothing
--     Just n1 -> case safeEval e2 of
--     Nothing -> Nothing
--     Just n2 -> Just (n1 + n2)
-- safeEval (Sub e1 e2) =
--     case safeEval e1 of
--     Nothing -> Nothing
--     Just n1 -> case safeEval e2 of
--     Nothing -> Nothing
--     Just n2 -> Just (n1 - n2)
-- safeEval (Mul e1 e2) =
--     case safeEval e1 of
--     Nothing -> Nothing
--     Just n1 -> case safeEval e2 of
--     Nothing -> Nothing
--     Just n2 -> Just (n1 * n2)
-- safeEval (Div e1 e2) =
--     case safeEval e1 of
--     Nothing -> Nothing
--     Just n1 -> case safeEval e2 of
--     Nothing -> Nothing
--     Just n2 -> if n2 == 0 then Nothing else Just (n1 `div` n2)

evalSeq :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
evalSeq mi f = case mi of
  Nothing -> Nothing
  Just a -> f a


safeEval :: Exp -> Maybe Integer
safeEval (Add e1 e2) =
  safeEval e1 `evalSeq` \n1 ->
  safeEval e2 `evalSeq` \n2 ->
  Just (n1+n2)
safeEval (Sub e1 e2) =
  safeEval e1 `evalSeq` \n1 ->
  safeEval e2 `evalSeq` \n2 ->
  Just (n1-n2)
safeEval (Mul e1 e2) =
  safeEval e1 `evalSeq` \n1 ->
  safeEval e2 `evalSeq` \n2 ->
  Just (n1*n2)
safeEval (Div e1 e2) =
  safeEval e1 `evalSeq` \n1 ->
  safeEval e2 `evalSeq` \n2 ->
  if n2 == 0
  then Nothing
  else Just (n1*n2)