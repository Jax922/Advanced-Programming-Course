-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the APpy APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Definitions
import Parser
import Transformer

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Appy tests : "
  [parserTests, transformTests, somkeTests]

somkeTests = testGroup "Smoke tests" [
  testCase "Parser" $ parseSpec str @?= Right ("", eg),
  testCase "Transformer.convert" $
    convert eg @?= Right g] -- assumes that convert preserves input rule order
  where
    str = "---\n S ::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
    str1 = "---\n Digit ::= @{?isDigit}{?>0} {digitToInt _1}.\n _ ::= {()}."
    eg = [(("S", RPlain, Nothing),
           EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1") (ESeq [ESimple (SLit "b")] "0")),
          (("_", RSep, Nothing), ESeq [] ("()"))]
    g = [(("S", RPlain, Nothing), 
          [([SNTerm "S", SLit "a"], AUser "_1+1"),
           ([SLit "b"], AUser "0")]),
         (("_", RSep, Nothing), [([], AUser "()")])]

-- ==================================== Appy Parser test part ===========================================
parserTests = testGroup "Parser tests" [
  testCase "basic test" $ 
    parseSpec "---\n Digit ::= @{?isDigit}{?>0} {digitToInt _1}.\n _ ::= {()}." @?= Right("", eg1),
  testCase "preamble test" $
    parseSpec "this is preamble---\n Digit ::= @{?isDigit}{?>0} {digitToInt _1}.\n _ ::= {()}." @?= Right("this is preamble", eg1),
  testCase "tokLit parser test" $ 
    parseSpec "---\n T ::= T \"a\" {_1}.\n _ ::= {()}." @?= Right("", eg2),
  testCase "@ parser test" $
    parseSpec "---\n T ::= T @ {_1}.\n _ ::= {()}." @?= Right("", eg3),
  testCase "charLit parser test" $
    parseSpec "---\n T ::= T 'a' {_1}.\n _ ::= {()}." @?= Right("", eg4),
  testCase "atom parser test" $
    parseSpec "---\n T ::= T 'a' \"b\" @ {_1}.\n _ ::= {()}." @?= Right("", eg5),
  testCase "htext parser test" $
    parseSpec "---\n T ::= T @{?isLetter} {_1}.\n _ ::= {()}." @?= Right("", eg6),
  testCase "simple ? parser test" $
    parseSpec "---\n T ::= T @ ? {_1}.\n _ ::= {()}." @?= Right("", eg7),
  testCase "simple * parser test" $
    parseSpec "---\n T ::= T @ * {_1}.\n _ ::= {()}." @?= Right("", eg8),
  testCase "! simple parser test" $
    parseSpec "---\n T ::= T ! @ {_1}.\n _ ::= {()}." @?= Right("", eg9),
  testCase "seq parser test" $
    parseSpec "---\n T ::= T E {_1}.\n _ ::= {()}." @?= Right("", eg10),
  testCase "alts parser test" $
    parseSpec "---\nS ::= S S2 {_1+1}. S2 ::= (A | B) {_1+1}.\n _ ::= {()}." @?= Right("", eg11),
  testCase "OptType parser test" $
    parseSpec "---\nS {: hh } ::= S S2 {_1+1}. S2 ::= (A | B) {_1+1}.\n _ ::= {()}." @?= Right("", eg12),
  testCase "ERule parser test" $
    parseSpec "---\nS {: hh } ::= S S2 {_1+1}. S2 ::= (A | B) {_1+1}.\n _ ::= {()}." @?= Right("", eg12),
  testCase "no preamble error test" $
    parseSpec "S {: hh } ::= S S2 {_1+1}. S2 ::= (A | B) {_1+1}.\n _ ::= {()}." @?= Left "parse error",
  testCase "no ending error test" $
    parseSpec "---\nS {: hh } ::= S S2 {_1+1}. S2 ::= (A | B) {_1+1}." @?= Left "parse error",
  testCase "no right error test" $
    parseSpec "---\nS {: hh } S S2 {_1+1}. S2 ::= (A | B) {_1+1}." @?= Left "parse error",
  testCase "no left error test" $
    parseSpec "---\n::= S {: hh } S S2 {_1+1}. S2 ::= (A | B) {_1+1}." @?= Left "parse error"
  ]
  where
    eg1 = [(("Digit",RPlain,Nothing),ESeq [EPred (EPred (ESimple SAnyChar) "isDigit") ">0"] "digitToInt _1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg2 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ESimple (SLit "a")] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg3 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ESimple SAnyChar] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg4 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ESimple (SChar 'a')] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg5 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ESimple (SChar 'a'),ESimple (SLit "b"),ESimple SAnyChar] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg6 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),EPred (ESimple SAnyChar) "isLetter"] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg7 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),EOption (ESimple SAnyChar)] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg8 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),EMany (ESimple SAnyChar)] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg9 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ENot (ESimple SAnyChar)] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg10 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ESimple (SNTerm "E")] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg11 = [(("S",RPlain,Nothing),ESeq [ESimple (SNTerm "S"),ESimple (SNTerm "S2")] "_1+1"),(("S2",RPlain,Nothing),ESeq [EBar (ESimple (SNTerm "A")) (ESimple (SNTerm "B"))] "_1+1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg12 = [(("S",RPlain,Just (AUser "hh ")),ESeq [ESimple (SNTerm "S"),ESimple (SNTerm "S2")] "_1+1"),(("S2",RPlain,Nothing),ESeq [EBar (ESimple (SNTerm "A")) (ESimple (SNTerm "B"))] "_1+1"),(("_",RSep,Nothing),ESeq [] "()")]

-- ==================================== Appy Transform test part ===========================================
transformTests = testGroup "Transfrom tests" [
  testCase "Transformer.convert straightforward test" $ 
    convert eg @?= Right g,
  testCase "Transformer.convert nested test" $ 
    convert eg1 @?= Right g1,
  testCase "Transformer.convert grammar error test" $ 
    convert eg2 @?= Left "Grammar is invalid, please check",
  testCase "Transformer.convert EOption test" $ 
    convert eg3 @?= Right g3,
  testCase "Transformer.convert EMany test" $ 
    convert eg4 @?= Right g4,
  testCase "Transformer.convert HText test" $
    convert eg5 @?= Right g5,
  testCase "Transformer.convert basic test" $
    convert eg6 @?= Right g6,
  testCase "Transformer.convert basic test" $
    convert eg7 @?= Left "Grammar is invalid, please check",
  testCase "Transformer.convert basic test" $
    convert eg8 @?= Right g8,
  testCase "Transformer.lre basic test" $
    lre g @?= Right lg1,
  testCase "Transformer.lre m=0 error test" $
    lre g9 @?= Left "contain no base case",
  testCase "Transformer.lre test" $
    lre g1 @?= Right lg,
  testCase "Transformer.lre test" $
    lre g3 @?= Right lg3,
  testCase "Transformer.lre test" $
    lre g4 @?= Right lg4,
  testCase "Transformer.lre test" $
    lre g5 @?= Right lg5,
  testCase "Transformer.lre test" $
    lre g6 @?= Right lg6,
  testCase "Transformer.lre test" $
    lre g8 @?= Right lg8
  ]
  where
    eg = [(("S", RPlain, Nothing),
           EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                      (ESeq [ESimple (SLit "b")] "0")),
          (("_", RSep, Nothing), ESeq [] ("()"))]
    g = [(("S", RPlain, Nothing), 
          [([SNTerm "S", SLit "a"], AUser "_1+1"),
           ([SLit "b"], AUser "0")]),
         (("_", RSep, Nothing), [([], AUser "()")])]
    eg1 = [(("S",RPlain,Nothing),ESeq [ESimple (SNTerm "S"),EBar (ESimple (SNTerm "A")) (ESimple (SNTerm "B"))] "_1+1"),(("_",RSep,Nothing),ESeq [] "()")]
    g1 =[(("S",RPlain,Nothing),[([SNTerm "S",SNTerm "ISCONVERTS2"],AUser "_1+1")]),(("ISCONVERTS2",RPlain,Nothing),[([SNTerm "A"],AUser "_1+1"),([SNTerm "B"],AUser "_1+1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    eg2 = [(("S",RPlain,Nothing),ESeq [ESimple (SNTerm "S"),EBar (ESimple (SNTerm "A")) (ESimple (SNTerm "B"))] "_1+1"),
            (("S",RPlain,Nothing),ESeq [ESimple (SNTerm "S"),EBar (ESimple (SNTerm "A")) (ESimple (SNTerm "B"))] "_1+1"),
              (("_",RSep,Nothing),ESeq [] "()")]
    eg3 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),EOption (ESimple SAnyChar)] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    g3 = [(("T",RPlain,Nothing),[([SNTerm "T",SAnyChar],AUser "_1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    eg4 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),EMany (ESimple SAnyChar)] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    g4 = [(("T",RPlain,Nothing),[([SNTerm "T",SAnyChar],AUser "_1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    eg5 = [(("S",RPlain,Nothing),ESeq [ESimple (SNTerm "S"),ESimple (SNTerm "S2")] "_1+1"),(("S2",RPlain,Nothing),ESeq [EBar (ESimple (SNTerm "A")) (ESimple (SNTerm "B"))] "_1+1"),(("_",RSep,Nothing),ESeq [] "()")]
    g5 = [(("S",RPlain,Nothing),[([SNTerm "S",SNTerm "S2"],AUser "_1+1")]),(("S2",RPlain,Nothing),[([SNTerm "ISCONVERTS22"],AUser "_1+1")]),(("ISCONVERTS22",RPlain,Nothing),[([SNTerm "A"],AUser "_1+1"),([SNTerm "B"],AUser "_1+1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    eg6 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),EPred (ESimple SAnyChar) "isLetter"] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    g6 = [(("T",RPlain,Nothing),[([SNTerm "T",SAnyChar],AUser "_1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    eg7 = [(("Digit",RPlain,Nothing),ESeq [EPred (EPred (ESimple SAnyChar) "isDigit") ">0"] "digitToInt _1"),(("_",RSep,Nothing),ESeq [] "()")]
    eg8 = [(("T",RPlain,Nothing),ESeq [ESimple (SNTerm "T"),ESimple (SLit "a")] "_1"),(("_",RSep,Nothing),ESeq [] "()")]
    g8 = [(("T",RPlain,Nothing),[([SNTerm "T",SLit "a"],AUser "_1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    lg1 = [(("S",RPlain,Nothing),[([SLit "b",SNTerm "S2"],AApp (AVar "_2") (AUser "0"))]),(("S2",RPlain,Nothing),[([SDummy,SLit "a",SNTerm "S2"],ALam "_1" (AApp (AVar "_3") (AUser "_1+1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]
    g9 = [(("S", RPlain, Nothing), 
          [([SNTerm "S"], AUser "_1+1"),
           ([SLit "b"], AUser "0")]),
         (("_", RSep, Nothing), [([], AUser "()")])]
    lg = [(("S",RPlain,Nothing),[]),(("S2",RPlain,Nothing),[([SDummy,SNTerm "ISCONVERTS2",SNTerm "S2"],ALam "_1" (AApp (AVar "_3") (AUser "_1+1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("ISCONVERTS2",RPlain,Nothing),[([SNTerm "A"],AUser "_1+1"),([SNTerm "B"],AUser "_1+1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    lg3 =  [(("T",RPlain,Nothing),[]),(("T2",RPlain,Nothing),[([SDummy,SAnyChar,SNTerm "T2"],ALam "_1" (AApp (AVar "_3") (AUser "_1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]
    lg4 = [(("T",RPlain,Nothing),[]),(("T2",RPlain,Nothing),[([SDummy,SAnyChar,SNTerm "T2"],ALam "_1" (AApp (AVar "_3") (AUser "_1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]
    lg5 = [(("S",RPlain,Nothing),[]),(("S2",RPlain,Nothing),[([SDummy,SNTerm "S2",SNTerm "S2"],ALam "_1" (AApp (AVar "_3") (AUser "_1+1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("S2",RPlain,Nothing),[([SNTerm "ISCONVERTS22"],AUser "_1+1")]),(("ISCONVERTS22",RPlain,Nothing),[([SNTerm "A"],AUser "_1+1"),([SNTerm "B"],AUser "_1+1")]),(("_",RSep,Nothing),[([],AUser "()")])]
    lg6 = [(("T",RPlain,Nothing),[]),(("T2",RPlain,Nothing),[([SDummy,SAnyChar,SNTerm "T2"],ALam "_1" (AApp (AVar "_3") (AUser "_1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]
    lg8 = [(("T",RPlain,Nothing),[]),(("T2",RPlain,Nothing),[([SDummy,SLit "a",SNTerm "T2"],ALam "_1" (AApp (AVar "_3") (AUser "_1"))),([SDummy],ALam "_1" (AVar "_1"))]),(("_",RSep,Nothing),[([],AUser "()")])]


















