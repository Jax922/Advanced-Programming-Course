-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute, rangeFun, printFun)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp $ \_ -> (Right a, [])
  m >>= f = Comp $ \e -> case runComp m e of
                            (Left err, s) -> (Left err, s)
                            (Right a, s) -> 
                              case runComp (f a) e of
                                (Left err, s') -> (Left err, s <> s')
                                (Right a', s'') -> (Right a', s <> s'')

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort err = Comp $ \_ -> (Left err, [])

look :: VName -> Comp Value
look vn = Comp $ \e -> case lookup vn e of
                            Nothing -> (Left (EBadVar vn), [])
                            Just x -> (Right x, [])

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding vn v cm = Comp $ \e -> runComp cm ((vn, v) : e )  

output :: String -> Comp ()
output s = Comp $ \_ -> (Right (), [s])

------------------ Helper Functions for core functions ----------------
-- this is a helper function for `Plus` `Minus` `Times` `Div` `Mod` operation
-- Input: two Value and a operation function
-- Return: running error message (String) or 
--        the operation result (IntVal number)
arithmeticOp :: Value -> Value -> (Int -> Int -> Either String Int)
                  -> Either String Value                        
arithmeticOp (IntVal v1) (IntVal v2) f = case f v1 v2 of
                                            Left err -> Left err
                                            Right i -> Right (IntVal i)
-- you cann not input any non-IntVal number
arithmeticOp _ _ _ = Left "err: can not support non-integer" 

-- this is a helper function for `eq` operation,
-- compare two Values for equality
-- Input: two Value
-- Return: TrueVal (if thay are equal), FalseVal(if they are not equality)
eqOp :: Value -> Value -> Either String Value
eqOp NoneVal NoneVal = Right TrueVal
eqOp TrueVal TrueVal = Right TrueVal
eqOp FalseVal FalseVal = Right TrueVal
eqOp (IntVal i1) (IntVal i2) =  if i1 == i2 
                                  then Right TrueVal
                                else Right FalseVal  
eqOp (StringVal s1) (StringVal s2) =  if s1 == s2 
                                        then Right TrueVal
                                      else Right FalseVal  
eqOp (ListVal l1) (ListVal l2) =  if l1 == l2 
                                    then Right TrueVal
                                  else Right FalseVal  
eqOp _ _ = Right FalseVal -- other cases return FalseVal


-- this is a helper function for `Less` or ` Greater` operation
-- Compare thefmagnitude of two numbers
-- Input: two Value and  compare function(`Less` or `Grater` function)
-- Return: running error message or the result of your compare function 
lessAndGreaterOp :: Value -> Value ->  (Int -> Int -> Bool) 
                      -> Either String Value
lessAndGreaterOp (IntVal v1) (IntVal v2) f =  if f v1 v2 
                                                then Right TrueVal
                                              else Right FalseVal
lessAndGreaterOp _ _ _ = Left "err: just support integer" -- you can not 
                                                          -- input non-IntVal

-- this is a helper function for `in` operation
-- Detemine whether the value in the list
-- Return:  TrueVal if the list cantain the Value, adversely, return FlaseVal
--          if second value is not ListVal, return a errror message
inOp :: Value -> Value ->  Either String Value 
inOp _ (ListVal []) = Right FalseVal
inOp x (ListVal (l:ls)) = case eqOp x l of
                            Right FalseVal -> inOp x (ListVal ls)
                            Right TrueVal -> Right TrueVal
                            Right _ -> inOp x (ListVal ls)
                            Left err -> Left err
inOp _ _ = Left "err: just support List" -- the second value must be ListVal

-- implement the `range` function of Boa
rangeFun :: [Value] -> Either RunError [Value]
rangeFun [IntVal v1] =  if v1 >= 0 
                          then Right [IntVal n|n<-[0..v1-1]]
                        else Right []
rangeFun [IntVal v1, IntVal v2] = if v1 <= v2 
                                    then Right [IntVal n|n<-[v1..v2-1]]
                                  else Right []
rangeFun [IntVal v1, IntVal v2, IntVal v3]
    | v3 > 0
    = if v1 > v2 then
          Right []
      else
          Right [IntVal n | n <- [v1, v1 + v3 .. v2]]
    | v3 < 0
    = if v1 < v2 then
          Right []
      else
          Right [IntVal n | n <- [v1, v1 + v3 .. v2 + 1]]
    | otherwise = Left (EBadArg "range function need v3 can not be 0")
rangeFun  _ = Left (EBadArg "range need int")

-- implemant the `print` function of Boa
printFun :: Value -> String
printFun NoneVal = "None"
printFun TrueVal = "True"
printFun FalseVal = "False"
printFun (IntVal i) = show i
printFun (StringVal s)  = s
printFun (ListVal []) = "[]"
printFun (ListVal ls) = "[" ++ listPrint (ListVal ls) ++ "]"

-- this is a helper function for print `ListVal` value 
listPrint :: Value -> String
listPrint (ListVal []) = "[]"
listPrint (ListVal [i]) = printFun i
listPrint (ListVal(l:ls)) = printFun l ++ ", " ++ listPrint (ListVal ls)
listPrint _ = "" -- can not print non-ListVal value

-- implement `List Comprehension` feature 
ccclauseEval :: Exp -> [CClause] -> Comp [Value]
ccclauseEval e [] = sequence[eval e]
ccclauseEval e (cc:ccs) = do
  case cc of 
    CCFor vn e' -> do
      x <- eval e'
      case x of
        (ListVal l) -> do
          res <- sequence([withBinding vn y (ccclauseEval e ccs)| y <-l ])
          return $ concat res
        _ -> abort (EBadArg "must be ListVal") -- the expression of CCFor body 
                                               -- must be a ListVal     
    CCIf e'' -> do
      x <- eval e''
      if truthy x then
        ccclauseEval e ccs 
      else return []
------------------------------------------------------------------------
--------------------- Core functions for interpreter -----------------
truthy :: Value -> Bool
truthy v
  | v == NoneVal = False
  | v == FalseVal = False
  | v == IntVal 0 = False
  | v == StringVal "" = False
  | v == ListVal [] = False
  | otherwise = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus x y = arithmeticOp x y (\x' y' -> Right (x'+y'))
operate Minus x y = arithmeticOp x y (\x' y' -> Right (x'-y'))
operate Times x y = arithmeticOp x y (\x' y' -> Right (x'*y'))  
operate Div x y = arithmeticOp x y (\x' y' ->  
                                      -- if the diviend number can't be zero
                                      if y' == 0 then Left "err:can't be zero" 
                                      else Right (x' `div` y'))
operate Mod x y = arithmeticOp x y (\x' y' ->  
                                      -- if the modulo number can't be zero
                                      if y' == 0 then Left "err:can't be zero"
                                      else Right (x' `mod` y'))
operate Eq x y = eqOp x y
operate Less x y = lessAndGreaterOp x y (<)
operate Greater x y = lessAndGreaterOp x y (>)
operate In x y = inOp x y

apply :: FName -> [Value] -> Comp Value
apply fn v 
  | fn =="range"  = case rangeFun v of 
                      Left err -> Comp $ \_ -> (Left err, [])
                      Right l -> Comp $ \_ -> (Right (ListVal l), [])
  | fn == "print" = Comp $ \_ -> (Right NoneVal, [unwords $ map printFun v])
  | otherwise = Comp $ \_->(Left (EBadFun fn ), []) --can't support other funcs 

--------- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = Comp $ \_ -> (Right v, [])
eval (Var vn) = Comp $ \e -> case lookup vn e of
                              --if can't find the variable will signal a error
                              Nothing -> (Left (EBadVar vn), []) 
                              Just x -> (Right x, [])
eval (Oper o x y) = do
                    x' <- eval x
                    y' <- eval y
                    case operate o x' y' of
                      Left err -> Comp $ \_ -> (Left (EBadArg err), [])
                      Right v -> Comp $ \_ -> (Right v, [])
eval (Not x) =  do
                x' <- eval x
                (if not (truthy x') then
                  Comp $ \ _ -> (Right TrueVal, [])
                else
                  Comp $ \ _ -> (Right FalseVal, []))
eval (Call fn v) =  do
                    v' <- mapM eval v
                    apply fn v'
eval (List []) = Comp $ \_ -> (Right (ListVal []), [])
eval (List x) = do
                x' <- mapM eval x
                Comp $ \_ -> (Right (ListVal x'), [])
eval (Compr x cc) = do
                    r <- ccclauseEval x cc
                    return (ListVal r)

exec :: Program -> Comp ()
exec [] = return ()
exec (x:xs) = case x of
                (SDef vn e) ->  do
                  v' <- eval e -- paresing a expression to get value
                  withBinding vn v' (exec xs) -- bind value to V-Name
                (SExp e) -> eval e >> exec xs

execute :: Program -> ([String], Maybe RunError)
execute p = let (a, str) = runComp (exec p) []
                in case a of
                  Left err -> ([x | x <- str, x /= ""], Just err) 
                  Right _ -> ([x | x <- str, x /= ""], Nothing)
