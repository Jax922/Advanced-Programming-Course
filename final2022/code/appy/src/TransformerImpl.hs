-- Put yor transformer implementation in this file
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TransformerImpl where

import Definitions
import Control.Monad.State
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))
import Data.Maybe (fromMaybe)
import Data.List ( group, sort, transpose )

type Parser = ReadP
type IsFromParser = Bool
type NestedSeqs = [ERHS]
type Seqs = [ERHS]
type FreshName = ERHS

type Alpha = [([Simple], Action)] --  𝛼-alternatives
type Beta = [([Simple], Action)] -- 𝛽-alternatives
type LFactor = [([Simple], Action)]
type NonLFactor = [([Simple], Action)]

-- Conversion
-- reference :https://stackoverflow.com/questions/2466484/converting-ebnf-to-bnf
convert :: EGrammar -> EM Grammar
convert g =
    case checkGrammar g of
      Left e -> Left e
      Right _ ->
        case cg of
            [] -> Left "non-Grammar"
            _ -> Right cg
        where cg = convertSf $ convertNested g

-- Left-recursion elimination (LRE)
lre :: Grammar -> EM Grammar
lre [] = Right []
lre (g:gs) =
  case helperLre g of
    Left e -> Left e
    Right r ->
      fmap (r ++) (lre gs)

-- Left-factorization
lfactor :: Grammar -> EM Grammar
lfactor [] = Right []
lfactor (g:gs) =
  case helperLFactor g of
    Left e -> Left e
    Right r ->
      fmap (r ++) (lre gs)


-- =================================
-- helper functions for convert part
-- =================================

-- check grammar
checkGrammar :: EGrammar -> EM Bool
checkGrammar g =
  if checkNTNameDefined && checkTokenSeparation && checkDefinedOnce
    then Right True
  else
    Left "Grammar is invalid, please check"
  where
    dnames = definedNames g
    tnames = totalNTNames g
    checkNTNameDefined = all (`elem` (tnames++["_"])) dnames -- all (`elem` dnames) 
    checkTokenSeparation = "_" `elem` dnames
    checkDefinedOnce = allUnique dnames

-- reference Data.List.Unique
sg :: Ord a => [a] -> [[a]]
sg = group . sort
allUnique :: [String] -> Bool
allUnique = all ( (==) 1 . length) . sg

definedNames :: EGrammar -> [NTName]
definedNames [] = []
definedNames (((n, _, _),_):xs) = n :definedNames xs

totalNTNames :: EGrammar -> [String]
totalNTNames [] = []
totalNTNames ((_, r):xs) =
  case r of
    ESimple (SNTerm n) -> n : totalNTNames xs
    ESeq es _ -> f es ++ totalNTNames xs
    EBar e1 e2 -> f [e1] ++ f [e2] ++ totalNTNames xs
    EOption eo -> f [eo] ++ totalNTNames xs
    EMany em -> f [em] ++ totalNTNames xs
    EPred ep _ -> f [ep] ++ totalNTNames xs
    ENot en -> f [en] ++ totalNTNames xs
  where
    f :: [ERHS] -> [String]
    f (s:ss) =
      case s of
        ESimple (SNTerm n) -> n : f ss
        ESimple _ -> f ss
        EBar e1 e2 -> f [e1] ++ f [e2] ++ f ss
        ESeq es _ -> f es ++ f ss
        EOption eo -> f [eo] ++ f ss
        EMany em -> f [em] ++ f ss
        EPred ep _ -> f [ep] ++ f ss
        ENot en -> f [en] ++ f ss
    f [] = []

-- ============convert straightforward==============
-- [(RLHS, ERHS)]
-- type Rule = (RLHS, [([Simple]{-seq-}, Action)]{-alts-})
prefixFreshName = "ISCONVERT"

convertSf :: EGrammar -> Grammar
convertSf [] = []
convertSf (((ntname, k, t), r):egs) = ((ntname, k, t), zip ss ac) : convertSf egs
  where 
    ss = convertMain r
    ac = getAction isFromParser r
    isFromParser = take (length prefixFreshName) ntname /= prefixFreshName
-- IsFromParser
convertMain :: ERHS -> [[Simple]]
convertMain (EBar es1 es2) = convertMain es1 ++ convertMain es2
convertMain e = [convertSfERHS e]

convertSfERHS :: ERHS -> [Simple]
-- convertSfERHS (EBar es1 es2) = convertSfERHS (ESeq [es1] "") ++ convertSfERHS (ESeq [es2] "") 
-- convertSfERHS b (ESeq es ht) = [([convertSfSimple e | e <- es ], convertHText b (extractHText ht))]
convertSfERHS (ESeq es ht) = concatMap convertSfERHS es
-- convertSfERHS (EBar e1 e2) = getAction b e1 ++ getAction b e2
convertSfERHS(EOption eo) = convertSfERHS eo
convertSfERHS(EMany em) = convertSfERHS em
convertSfERHS (EPred ep _) = convertSfERHS ep
convertSfERHS(ENot en) =  convertSfERHS en
convertSfERHS (ESimple s) =[s]


-- convertSfERHS _ _ = []

getAction :: IsFromParser -> ERHS -> [Action]
getAction b (ESeq _ ht) = [convertHText b (extractHText ht)]
getAction b (EBar e1 e2) = getAction b e1 ++ getAction b e2
getAction b (EOption eo) = getAction b eo
getAction b (EMany em) = getAction b em
getAction b (EPred ep _) = getAction b ep
getAction b (ENot en) =  getAction b en
getAction b (ESimple s) =  [AUser ""]
-- getAction b EBar e1 e2 = 
 
convertSfESeq :: IsFromParser -> ERHS -> ([Simple], Action)
convertSfESeq b (ESeq es ht) = ([convertSfSimple e | e <- es ], convertHText b (extractHText ht))

convertSfSimple :: ERHS -> Simple
convertSfSimple (ESimple s) = s

-- ======= convert HText to Action ========
convertHText :: IsFromParser -> [HText] -> Action
convertHText b [h] = AUser h
convertHText b (h:hs) =
  AApp (helperConvertHText b h) (convertHText b hs)

-- helper function for convertHText
helperConvertHText :: IsFromParser -> HText -> Action
helperConvertHText _ ":" = ACst "(:)"
helperConvertHText _ "+" = ACst "(+)"
helperConvertHText _ "-" = ACst "(-)"
helperConvertHText _ "*" = ACst "(*)"
helperConvertHText _ "/" = ACst "(/)"
helperConvertHText b ht =
  case head ht of
    '_' -> if b then AUser ht else AVar ht
    '(' -> AUser ht
    _ -> ACst ht

-- convert a HText string to a list of HText
extractHText :: HText -> [HText]
extractHText = extractColon . extract . words

-- conbine content in parenthesis after words
-- for example : "Add _1 (Negate _3 _4) (Add _5 _6)" ->  ["Add","_1","(Negate _3 _4)","(Add _5 _6)"] 
--  step1 : words "Add _1 (Negate _3 _4) (Add _5 _6)" =  ["Add","_1","(Negate","_3","_4)","(Add","_5","_6)"]
--  step2: extract ["Add","_1","(Negate","_3","_4)","(Add","_5","_6)"] = ["Add","_1","(Negate _3 _4)","(Add _5 _6)"]
extract :: [String] -> [String]
extract [] = []
extract ["()"] = ["()"]
extract (s:ss) =
  case head s of
    '(' ->  (s ++" "++fst xs) : extract(snd xs)
    _ -> s : extract ss
    where xs = extractTail ss

extractTail :: [String] -> (String, [String])
extractTail [] = ("", [])
extractTail (s:ss) =
  case last s of
    ')' -> (s, ss)
    _ -> (s ++" "++fst t, snd t)
    where t = extractTail ss

splitAtColon :: String -> [String]
splitAtColon s =  case dropWhile (==':') s of
                      "" -> []
                      s' -> if s'' == "" then w : splitAtColon s'' else w :":": splitAtColon s''
                            where (w, s'') = break (==':') s'

extractColon :: [String] -> [String]
extractColon = concatMap splitAtColon


-- ======= convert Nested ========
-- convert nested EBar or ESeq in a ESeq
seqNested :: ERHS -> Bool
seqNested (ESeq _ _) = True
seqNested (EBar _ _) = True
seqNested _ = False

containNested :: ERHS -> Bool
containNested (ESeq xs _) = any seqNested xs
containNested (EBar x y) = containNested x || containNested y
containNested _ = False

-- [ESeq]
seqFilterNested :: [ERHS] -> [ERHS]
seqFilterNested = filter seqNested

-- create primes
primes = filterPrime [2..200]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
        filterPrime [] = []

-- create fresh names
createFreshNames :: String -> Int -> ([NTName], [FreshName])
createFreshNames s n =
  (ns, fs)
  where
    ns = map (\c -> prefixFreshName ++ s ++ show c) (take n primes)
    fs = map (ESimple . SNTerm) ns

convertElement :: String -> [ERHS] -> ([ERHS], [(NTName, ERHS)])
convertElement s xs =
  (map (`convertItem` fnameWithSeq) xs, seqWithName)
  where
    fs = seqFilterNested xs
    (x, y) = createFreshNames s (length fs)
    fnameWithSeq = zip fs y
    seqWithName = zip x fs

convertItem :: ERHS -> [(ERHS, ERHS)] -> ERHS
convertItem x xs =
  fromMaybe x (lookup x xs)

helperCovertESeq :: HText -> ERHS -> ERHS
helperCovertESeq ht (EBar e1 e2) = EBar (ESeq [e1] ht) (ESeq [e2] ht)


convertNested :: [ERule] -> [ERule]
convertNested [] = []
convertNested (((s, j, t), r) : rss) =
    case r of
      ESeq rs ht ->
        r': [ ((x, j, t), helperCovertESeq ht y)| (x, y) <-sns] ++ convertNested rss
        where
          (rs', sns) = convertElement s rs -- try try
          r' = ((s, j, t), ESeq rs' ht)
      EBar r1 r2 ->
        er': [ ((x, j, t), y)| (x, y) <-nss]++ convertNested rss
        where
          (r1', ns1') = convertBar s r1
          (r2', ns2') = convertBar s r2
          nss = ns1' ++ ns2'
          er' = ((s, j, t),EBar r1' r2')
      EPred ep ht ->
        ep':[((x, j, t), y)| (x, y) <-ns'] ++ convertNested rss
        where
          (e', ns') = convertBar s ep
          ep' = ((s, j, t),EPred e' ht)
      ENot en ->
        en':[((x, j, t), y)| (x, y) <-ns'] ++ convertNested rss
        where
          (e', ns') = convertBar s en
          en' = ((s, j, t),ENot e')
      EOption eo ->
        ro : ((fn, j, t), eo') : convertNested rss
        where
          (fn, eo') = convertOption s eo
          ro = ((s, j, t), ESimple $ SNTerm fn)
      EMany em ->
        rm : ((fn, j, t), em') : convertNested rss
        where
          (fn, em') = convertMany s em
          rm = ((s, j, t), ESimple $ SNTerm fn)
      _ -> ((s, j, t), r) : convertNested rss


convertBar :: String -> ERHS -> (ERHS, [(NTName, ERHS)])
convertBar s (ESeq xs ht) =
  (ESeq xs' ht, ns')
  where (xs', ns') = convertElement s xs
convertBar s (EBar r1 r2) =
  (EBar r1' r2', ns1'++ns2')
  where
    (r1', ns1') = convertBar s r1
    (r2', ns2') = convertBar s r2
convertBar s (EOption eo) =
  (ESimple $ SNTerm fn, [(fn, eo')])
  where
    (fn, eo') = convertOption s eo
convertBar s (EMany em) =
  (ESimple $ SNTerm fn, [(fn, em')])
  where
    (fn, em') = convertMany s em
convertBar s (EPred ep ht) =
  (EPred ep' ht, ns)
  where
    (ep', ns) = convertBar s ep
convertBar s (ENot en) =
  (ENot en', ns)
  where
    (en', ns) = convertBar s en
convertBar s (ESimple sim) = (ESimple sim, [])

convertOption :: String -> ERHS -> (NTName, ERHS)
convertOption s e =
    (fname, EBar (f e) nothing)
  where
    fname = s ++ "O" ++ show (head primes)
    nothing = ESimple $ SNTerm "Nothing"
    f x =
      case x of
        EBar x1 x2 -> EBar (f x1) (f x2)
        _ ->  EPred  x "Just _1"

convertMany :: String -> ERHS -> (NTName, ERHS)
convertMany s e =
  (fname, EBar (f e) (ESeq [] "[]"))
  where
    fname = s ++ "M" ++ show (head primes)
    f :: ERHS -> ERHS
    f x =
      case x of
        ESeq es ht -> ESeq (es++[ESimple (SNTerm fname)]) (ht++":_"++show (length es +1))
        _ -> ESeq [x, ESimple $ SNTerm fname] "_1:_2"

-- =====================================================================================
-- helper functions for lre
-- =====================================================================================
z = "---\nE ::= E \"+\" num {Plus _1 (Cst _3)} | var {Var _1}.\n _ ::= {()}."
zz = ("",[(("E",RPlain,Nothing),EBar (ESeq [ESimple (SNTerm "E"),ESimple (SLit "+"),ESimple (SNTerm "num")] "Plus _1 (Cst _3)") (ESeq [ESimple (SNTerm "var")] "Var _1")),(("_",RSep,Nothing),ESeq [] "()")])
zg = [(("E",RPlain,Nothing),[([SNTerm "E",SLit "+",SNTerm "num"],AApp (ACst "Plus") (AApp (AUser "_1") (AUser "(Cst _3)"))),
        ([SNTerm "var"],AApp (ACst "Var") (AUser "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]
helperLre :: Rule -> EM Grammar
helperLre ((n,k,t), ss) =
  case containLrePlus n ss of
    False -> Right [((n,k,t), ss)]
    True ->
      if haveNoBaseCase
        then Left "contain no base case"
      else
        Right [createBetaRules fn r beta, createAlphaRules fn r alpha]
      where
        r = (n,k,t)
        (alpha, beta) = transformLre n ss
        haveNoBaseCase = containNoBaseCase alpha
        fn  = n ++ show (head primes)

createAlphaRules :: NTName ->RLHS -> Alpha -> Rule
createAlphaRules fn r [] = (r, [])
createAlphaRules fn r (a:as) =
  ((fn, k, t), [(ss', newAc)] ++ tails ++ dummy)
  where
    (ss, ac) = a
    (n,k,t) = r
    newNonT = SNTerm fn
    htext = AUser (convertActionToHText ac)
    v = "_"++show (length ss+1)
    newAc = ALam "_1" (AApp (AVar v) htext)
    dummy = [([SDummy], ALam "_1" (AVar "_1"))]
    ss' = [SDummy] ++ tail ss ++ [newNonT]
    tails = snd $ createAlphaRules fn r as


createBetaRules :: NTName ->RLHS -> Beta -> Rule
createBetaRules fn r [] = (r, [])
createBetaRules fn r (a:as) =
  (r, (ss', newAc) : tails)
  where
    (ss, ac) = a
    newNonT = SNTerm fn
    ss' = ss ++ [newNonT]
    htext = AUser (convertActionToHText ac)
    v = "_"++show (length ss+1)
    newAc = AApp (AVar v) htext
    tails =  snd $ createBetaRules fn r as


convertActionToHText :: Action -> String
convertActionToHText (AUser ht) = ht
convertActionToHText (AVar s) = s
convertActionToHText (ALam s ac) = s ++" "++ convertActionToHText ac
convertActionToHText (AApp ac1 ac2) = convertActionToHText ac1 ++" "++ convertActionToHText ac2
convertActionToHText (ACst s) = s

-- check no base case
containNoBaseCase :: Alpha -> Bool
containNoBaseCase = any isNoBaseCase

isNoBaseCase :: ([Simple], Action) -> Bool
isNoBaseCase (ss, _) = length ss <= 1
--
transformLre :: NTName -> [([Simple], Action)] -> (Alpha, Beta)
transformLre n [] = ([],[])
transformLre n (r:rs) =
   case s of
    SNTerm sn -> if sn == n then (r : a, b) else (a, r : b)
    _ -> (a, r : b)
  where
    (s:_, _) = r
    (a, b) = transformLre n rs

isLre :: NTName -> Simple -> Bool
isLre n (SNTerm s) = n == s
isLre _ _ = False

containLre :: NTName -> [Simple] -> Bool
containLre n = any (isLre n)

containLrePlus :: NTName -> [([Simple], Action)] -> Bool
containLrePlus n [] = False
containLrePlus n ((s,_):ss) = containLre n s || containLrePlus n ss


-- =====================================================================================
-- helper functions for lfactor
-- =====================================================================================
lf = [(("E",RPlain,Nothing),[([SNTerm "E",SLit "+",SNTerm "num"],AApp (ACst "Plus") (AApp (AUser "_1") (AUser "(Cst _3)"))),
        ([SNTerm "E", SNTerm "var"],AApp (ACst "Var") (AUser "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]

lf' = [[SNTerm "E",SLit "+",SNTerm "num"],[SNTerm "E",SNTerm "E"]]
helperLFactor :: Rule -> EM Grammar
helperLFactor ((n,k,t), ss) =
 case isLFactor ss' of
    False -> Right [((n,k,t), ss)]
    True ->
       Right [((n,k,t), ss)] -- need to do flactor 
  where
    ss' = extractSimpleList ss


-- check if a Rule have lfactor 
isLFactor :: [[Simple]] -> Bool
isLFactor [] = False
isLFactor ss =
  any ((>1) . (\x -> length (filter (== x) fl'))) fl'
  where
    fl = head $ transpose ss
    fl' = filter (/= SDummy) fl

extractSimpleList :: [([Simple], Action)] -> [[Simple]]
extractSimpleList rs = [ss | (ss, _) <- rs]

-- extract lfactor
extractLFactor :: [([Simple], Action)] -> (LFactor, NonLFactor, Int)
extractLFactor [] = ([],[], 0)
extractLFactor rs =
  ([x | (_, x) <- lfs], [y | (_, y) <- nonlfs], num)
  where
    ss = extractSimpleList rs
    fl = head $ transpose ss
    ns = map (\x -> if x == SDummy then 0 else length (filter (== x) fl) - 1) fl
    num= head (snd $ break (>0) ns)
    rs' = zip ns rs
    lfs = filter (\x -> fst x == num) rs'
    nonlfs = filter (\x -> fst x /= num) rs'
    -- [lfs, nonlfs] = filter (\x -> fst x == num) rs'

-- calculateNumFactor :: [[Simple]] -> Int
-- calculateNumFactor [] = 0
-- calculateNumFactor (s:ss) = 
--   foldl (\x y -> if f x == f y then ) 1
--   where
--     let f s = map (\x -> if x == SDummy then 0 else length (filter (== x) s) - 1) s

-- createLFactor :: Int -> NTName -> RLHS -> LFactor -> Rule
-- createLFactor n na r lf = 
--   (r, [])
--   where
--     (_, k, t) = r
--     r' = (na, k, t)
--     drop 
