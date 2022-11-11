module Generator (render, prelude) where

import Definitions
import Data.Char (isUpper)
import Data.List (intercalate)

gp p = "p_" ++ p   -- added in front of all generated parsers
rp f = "RP." ++ f  -- added in front of all ReadP functions

renderH :: HText -> String
renderH h = "{-U-}(" ++ h ++ ")" -- explicitly tag user-supplied code

renderA :: Action -> String
renderA (AUser h) = renderH h
renderA (ACst s) = s
renderA (AVar x) = x
renderA (ALam x a) = "(\\" ++ x ++ " -> " ++ renderA a ++ ")"
renderA (AApp a1 a2) = "(" ++ renderA a1 ++ " " ++ renderA a2 ++ ")"

renderS :: Simple -> String
renderS (SLit s) =
  "do {" ++ rp "string" ++ " " ++ show s ++ "; " ++ gp "_" ++ " " ++ show s ++"}"
renderS (SNTerm s) = gp s
renderS SAnyChar = rp "get"
renderS (SChar c) = rp "char" ++ " " ++ show c
renderS (SNot s) = "nfb_ (" ++ renderS s ++ ")"
renderS (SPred s h) = "pred_ " ++ renderS s ++ " " ++ renderH h
renderS SDummy = "return (error \"dummy\")"

renderR :: Rule -> String
renderR ((nt,rk,mt), alts) =
  let rbind p i = "_" ++ show i ++ " <- " ++ renderS p
      rseq (ps, r) =
        "do {" ++ intercalate "; " (zipWith rbind ps [1..]) ++
        "; return_ " ++ renderA r ++ "}"
      rhs = case map rseq alts of
              [s] -> s
              ss -> rp "choice" ++ " [" ++ intercalate ", " ss ++ "]"
      exp = case rk of
              RPlain -> rhs
              RToken -> "do r <- " ++ rhs ++ "; " ++ gp "_" ++ " \"\"; return r"
              RSep -> "\\_0 -> " ++ rhs
      pn = gp nt
      sig = case mt of
              Nothing -> ""
              Just tp -> pn ++ " :: Parser_ " ++ renderA tp ++ "\n"
  in sig ++ pn ++ " = " ++ exp

render :: Grammar -> String
render rs = intercalate "\n" (map renderR rs)

prelude =
  "type Parser_ a = " ++ rp "ReadP" ++ " a\n" ++
  "return_ :: a -> Parser_ a -- sometimes req'd for overloading resolution\n" ++
  "return_ = return\n" ++
  "nfb_ :: Parser_ a -> Parser_ () -- notFollowedBy\n" ++
  "nfb_ p = do b <- (p >> return True) " ++ rp "<++" ++ " return False\n" ++
  "            if b then " ++ rp "pfail" ++ " else return ()\n" ++
  "pred_ :: Parser_ a -> (a -> Bool) -> Parser_ a\n"++
  "pred_ p f = do a <- p; if f a then return a else " ++ rp "pfail" ++ "\n" ++
  "parseTop p s = case " ++ rp "readP_to_S" ++ " (do " ++
    gp "_" ++ " \"\"; a <- p; " ++ rp "eof" ++ "; return a) s of\n"++
  "  [] -> Left \"no parse\"\n"++
  "  [(a,_)] -> Right a\n"++
  "  _ -> Left \"ambiguous\"\n\n"
