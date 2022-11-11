-- Do not modify anything in this file!
module Definitions where

type ErrMsg = String    -- human-readable error messages
type EM = Either ErrMsg -- can be used directly as a Monad

type EGrammar = [ERule]
type ERule = (RLHS, ERHS)

type RLHS = (NTName, RKind, Maybe Type)
type NTName = String

data ERHS =
    ESimple Simple
  | ESeq [ERHS] HText
  | EBar ERHS ERHS
  | EOption ERHS
  | EMany ERHS
  | EPred ERHS HText
  | ENot ERHS
  deriving (Eq, Show, Read)

data Simple =
    SLit String
  | SNTerm String
  | SAnyChar
  | SChar Char
  | SNot Simple
  | SPred Simple HText
  | SDummy
  deriving (Eq,Show,Read)

data RKind = RPlain | RToken | RSep
  deriving (Eq,Show,Read)


data Action =
    AUser HText
  | AVar String
  | ALam String Action
  | AApp Action Action
  | ACst String
  deriving (Eq,Show,Read)

type HText = String   -- Haskell text from user
type Type = Action


type Grammar = [Rule]
type Rule = (RLHS, [([Simple]{-seq-}, Action)]{-alts-})
