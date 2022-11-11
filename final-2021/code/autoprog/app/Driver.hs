-- Driver module. You may modify this for your own experiments if you want
-- (as long as it doesn't break the build), but we will not use it for anything.

import Defs
import Parser (parseStringTDeclz, parseStringType)
import Resolver (declare, resolve)
import Coder (produce, solutions)

import System.Exit (die)
import System.Environment (getArgs)
import Text.Read (readEither)
import Data.List (intercalate)

data Params = Params {maxNodes, maxSols :: Int, decls :: [TDecl]}

params0 :: Params
params0 = Params {maxNodes = 1000, maxSols = 10, decls = []}

usage :: String
usage =
  "Usage: AutoProg OPTION... \"TYPE\"\n\
  \  where each OPTION can be one of\n\
  \    -d TDECLS      ';'-separated type/newtype/data decls\n\
  \    -f FNAME.hs    name of Haskell source file with such decls\n\
  \    -m MAXSOLS     max # of solutions to report (default: "
                         ++ show (maxSols params0) ++")\n\
  \    -n MAXNODES    max # of search-tree nodes to inspect (default: "
                        ++ show (maxNodes params0) ++")\n\
  \  and TYPE is the desired (polymorphic) Haskell type\n"

main = do ss <- getArgs; doArgs ss params0

doArgs :: [String] -> Params -> IO ()
doArgs ("-d":sn:ss) params =
  do ds <- fromEM "parsing decls from cmd line" $ parseStringTDeclz sn
     doArgs ss (params {decls = decls params ++ ds})
doArgs ("-f":fn:ss) params =
  do sn <- readFile fn
     ds <- fromEM "parsing decls from file" $ parseStringTDeclz sn
     doArgs ss (params {decls = decls params ++ ds})
doArgs ("-m":s:ss) params =
  do m <- fromEM "parsing -m argument" $ readEither s
     doArgs ss (params {maxSols = m})
doArgs ("-n":s:ss) params =
  do n <- fromEM "parsing -n argument" $ readEither s
     doArgs ss (params {maxNodes = n})
doArgs (('-':s):_) _params = putStrLn $ "bad option: -" ++ s
doArgs [s] params =
  do pt <- fromEM "parsing type" $ parseStringType s
     tce <- fromEM "resolving decls" $ declare (decls params)
     t <- fromEM "resolving type" $ resolve tce (return . STVar) pt
     let etree = produce [] t
     let es = solutions etree (maxNodes params)
                (Just $ Var "*** search limit hit")
     mapM_ (putStrLn . pp) (take (maxSols params) es)
doArgs _ params = putStr usage

fromEM :: String -> EM a -> IO a
fromEM s (Right a) = return a
fromEM s (Left e) = die $ "Error when " ++ s ++ ": " ++ e

pp :: Exp -> String
pp (Fst e) = "fst " ++ ppA e
pp (Snd e) = "snd " ++ ppA e
pp (App e1 e2) = pp e1 ++ " " ++ ppA e2
pp (Lam x e) = "\\" ++ x ++ " -> " ++ pp e
pp (RSel fn e) = fn ++ " " ++ ppA e
pp (RCons rn []) = rn
pp (RCons rn [(_,e)]) = rn ++ " " ++ ppA e
pp (RCons rn fes) =
  rn ++ " {" ++ intercalate ", " (map (\(f,e) -> f ++ " = " ++ pp e) fes) ++ "}"
pp e = ppA e

ppA (Var v) = v
ppA (Pair e1 e2) = "(" ++ pp e1 ++ ", " ++ pp e2 ++ ")"
ppA e = "(" ++ pp e ++ ")"

