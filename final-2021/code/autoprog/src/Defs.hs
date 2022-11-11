-- General type definitions for AutoProg. Do not modify anything!
module Defs where

-- General error-reporting type

type EM a = Either ErrMsg a

type ErrMsg = String

-- Primarily Parser-related definitions

data PType = PTVar TVName
           | PTApp TCName [PType]
  deriving (Eq, Show, Read)

data TDecl = TDSyn TDHead PType
           | TDRcd TDHead RCName [(FName, PType)]
  deriving (Eq, Show, Read)

type TDHead = (TCName, [TVName])

type TCName = String  -- type constructor
type RCName = String  -- record constructor
type TVName = String  -- type variable
type FName = String   -- record field

-- Primarily Resolver-related definitions

data SType = STVar TVName
           | STProd SType SType
           | STArrow SType SType
           | STRcd RCName [(FName, SType)]
  deriving (Eq, Show, Read)

type TCEnv = [(TCName, [SType] -> EM SType)]

-- initial type-constructor environment for built-in type constructors
tce0 :: TCEnv
tce0 =
  [("(,)", \ts -> case ts of [t1,t2] -> return $ STProd t1 t2
                             _ -> Left "bad args for (,)"),
   ("(->)", \ts -> case ts of [t1,t2] -> return $ STArrow t1 t2
                              _ -> Left "bad args for (->)")]

-- Primarily Coder-related definitions

data Tree a = Found a | Choice [Tree a]
  deriving (Eq, Show, Read)

data Exp = Var VName
         | App Exp Exp
         | Lam VName Exp
         | Pair Exp Exp
         | Fst Exp
         | Snd Exp
         | RCons RCName [(FName, Exp)]
         | RSel FName Exp
  deriving (Eq, Show, Read)

type VName = String   -- value variable
