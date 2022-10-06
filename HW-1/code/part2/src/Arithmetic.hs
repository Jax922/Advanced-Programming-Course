-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv, 
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

---------------------------  helper  for question #1 and #2 --------------------------
{-
  if dividend number is zero then signal a error message
-}
isDivZero :: Integer -> Integer
isDivZero i
  | i == 0    = error "dividend number can not be zero!!"
  | otherwise = i

{-
  In 'showExp' and 'evalSimple' functions, can not support " If, Var, Let, Sum " in a Exp expression,
  So, when Exp expression contain " If, Var, Let, Sum " will singal this error message.
-}
errMsg = "This operation is not support now!!"

---------------------------------------------------------------------------------------
--------------------------- question #1

showExp :: Exp -> String
showExp (Cst n) =  if n > 0 then show n else "(" ++ show n ++ ")"
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++")"
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y ++")"
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y ++")"
showExp (Div x y) = "(" ++ showExp x ++ "`div`" ++ showExp y ++")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++")"
showExp (If {}) = error errMsg
showExp (Var _) = error errMsg
showExp (Let {}) = error errMsg
showExp (Sum{}) = error errMsg

evalSimple :: Exp -> Integer
evalSimple (Cst i) = i
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) = evalSimple x `div` isDivZero (evalSimple y)
evalSimple (Pow x y) = if a==a && b >= 0 then a ^ b else error "Pow-operation is non-negative!!"
                        where a = evalSimple x
                              b = evalSimple y
evalSimple (If {}) = error errMsg
evalSimple (Var _) = error errMsg
evalSimple (Let {}) = error errMsg
evalSimple (Sum {}) = error errMsg

---------------------------------------------------------------------------------------
--------------------------- question #2

{-
  Input a `VName` and a `Integer`, it will create a new Env in which will bind the `Vname` with the `Integer`
-}
createEnv :: VName -> Integer -> Env
createEnv v n = \_v -> if _v == v then Just n else Nothing

{-
  Input a `Env` list and a `VName`, it will lookup the `VName` in this `Env` list.
  this is a helper  function for the below `extendEnv` function. 
-}
envList :: [Env] -> VName -> Maybe Integer
envList [] _ = Nothing
envList (e:es) v = case e v of
                      Just n -> Just n
                      Nothing -> envList es v

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = envList [\_v -> if _v == v then Just n else Nothing,  r]

evalFull :: Exp -> Env -> Integer
evalFull (If t y n) e  =  if evalFull t e /= 0 then evalFull y e else  evalFull n e
evalFull (Var v)    e  =  case  e v of
                            Just n -> n
                            Nothing -> error "can not find this var"
evalFull (Let v d b) e = evalFull b (extendEnv v (evalFull d e) e)
evalFull (Sum v f t b) e = if evalFull f e > evalFull t e 
                              then 0 
                           else sum $ map (evalFull b . createEnv v) [(evalFull f e) .. (evalFull t e)]
evalFull (Cst i) _ = i
evalFull (Add x y) e = evalFull x e + evalFull y e
evalFull (Sub x y) e = evalFull x e - evalFull y e
evalFull (Mul x y) e = evalFull x e * evalFull y e
evalFull (Div x y) e = evalFull x e `div` isDivZero (evalFull y e)
evalFull (Pow x y) e = if a==a && b >= 0 then a ^ b else error "Pow-operation is non-negative!!"
                        where a = evalFull x e
                              b = evalFull y e

---------------------------------------------------------------------------------------
--------------------------- question #3 
--------------------------- helper fcuntions for question #3 --------------------------

{-
  Check one variable is belong to one Env.
-}
eBadVarErrCheck :: VName -> Env -> Either ArithError Integer
eBadVarErrCheck v e = 
    case e v of 
      Just n -> Right n
      Nothing -> Left (EBadVar v)

{-
  This is find errors helper function.
  parameters definiation:
  x, y, z :: Either ArithError Integer
  f :: a function to handle the results of the x,y,z
-}
eHelper :: Either ArithError Integer -> Either ArithError Integer 
              -> (Integer -> Integer ->  Either ArithError Integer) -> Either ArithError Integer
eHelper x y f = case x of 
                  Left ae -> Left ae
                  Right x' -> 
                    case y of 
                      Left ae -> Left ae
                      Right y'-> f x' y'
---------------------------------------------------------------------------------------

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst i) _ = Right i
evalErr (Var v) e = eBadVarErrCheck v e
evalErr (Div x y) e = eHelper (evalErr x e) (evalErr y e) (\ x y -> if y == 0 then Left EDivZero else Right (x `div` y))
evalErr (Add x y) e = eHelper (evalErr x e) (evalErr y e) (\ x y -> Right (x + y))
evalErr (Sub x y) e = eHelper (evalErr x e) (evalErr y e) (\ x y -> Right (x - y))
evalErr (Mul x y) e = eHelper (evalErr x e) (evalErr y e) (\ x y -> Right (x * y))
evalErr (Pow x y) e = eHelper (evalErr x e) (evalErr y e) (\ x y -> if y < 0 then Left ENegPower else Right (x^y))
evalErr (Sum v f t b) e = eHelper (evalErr f e) (evalErr t e) (\ x y -> if x > y then Right 0 else Right (sum $ map (evalFull b . createEnv v) [x..y]))
evalErr (If t y n) e = eHelper (evalErr t e) (Right 0) (\ x _ -> if x /= 0 then evalErr y e else evalErr n e )
evalErr (Let v d b) e = case evalErr b initEnv of  -- Frist prior to check the body expression (named 'b')
                          Right n -> Right n -- if 'b' get 'Right n' means this 'Let' expression has result, so just return this result
                          Left _ -> 
                            case evalErr d e of
                              Left ae -> Left ae
                              Right x ->
                                case evalErr b (extendEnv v x e) of
                                  Left ae -> Left ae
                                  Right y -> Right y


-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined