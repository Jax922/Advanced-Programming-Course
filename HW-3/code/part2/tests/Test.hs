-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Text.ParserCombinators.ReadP
import Test.Tasty
import Test.Tasty.HUnit

-- newtype ParseError
--     = PE String
--     deriving (Eq, Show)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Boa Parse tests : "
  [identTests, numConstTests, stringConstTests, ntfTests, 
    notTest, parenthesisExprTest, identExprzTest, operTest,
      complexOperTest, forClauseTest, ifClauseTest, 
        listExprzTest, lsitForClauseTest, boaParseTest]

identTests :: TestTree
identTests = testGroup "ident tests : " [ 
  testCase "normal string" $
    parseString "varname" @?= Right [SExp (Var "varname")],
  testCase "normal string add numbers" $
    parseString "varname1234" @?= Right [SExp (Var "varname1234")],
  testCase "normal string contain underscore" $
    parseString "varname_1234" @?= Right [SExp (Var "varname_1234")],
  testCase "first char is a number" $
    (case parseString "1234varname" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p),
  testCase "reserved words" $
    case parseString "for" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

numConstTests :: TestTree
numConstTests = testGroup "numConst tests : " [ 
  testCase "normal number" $
    parseString "100" @?= Right [SExp (Const (IntVal 100))],
  testCase "negative number" $
    parseString "-100" @?= Right [SExp (Const (IntVal (-100)))],
  testCase "spaces in number" $
    (case parseString "1 00" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p),
  testCase "+2" $
    (case parseString "+2" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p),
  testCase "007" $
    case parseString "007" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

stringConstTests :: TestTree
stringConstTests = testGroup "stringConst tests : " [ 
  testCase "normal string" $
    parseString "'string'" @?= Right [SExp (Const (StringVal "string"))],
  testCase "\\n" $
    parseString "'\nstring'" @?= Right [SExp (Const (StringVal "\nstring"))],
  testCase "\\'" $
    parseString "'\'string'" @?= Right [SExp (Const (StringVal "'string"))],
  testCase "'fo\\o\'" $
    parseString "'fo\\o\'" @?= Right [SExp (Const (StringVal "foo"))]]

-- ‘None’ | ‘True’ | ‘False’ test
ntfTests :: TestTree
ntfTests = testGroup "‘None’ | ‘True’ | ‘False’ tests : " [ 
  testCase "None" $
    parseString "None" @?= Right [SExp (Const NoneVal)],
  testCase "True" $
    parseString "True" @?= Right [SExp (Const TrueVal)],
  testCase "False" $
    parseString "False" @?= Right [SExp (Const FalseVal)]]

-- ‘not’ Expr test
notTest :: TestTree
notTest = testGroup "not tests : " [ 
  testCase "not True" $
    parseString "not True" @?= Right [SExp (Not (Const TrueVal))],
  testCase "not False" $
    parseString "not False" @?= Right [SExp (Not (Const FalseVal))],
  testCase "not (not (x < 3))" $
    parseString "not (not (x < 3))" @?= Right [SExp (Not (Not (Oper Less (Var "x") (Const (IntVal 3)))))]]

-- ‘(’ Expr ‘)’ test
parenthesisExprTest :: TestTree
parenthesisExprTest = testGroup "( Expr ) tests : " [ 
  testCase "(True)" $
    parseString "(True)" @?= Right [SExp (Const TrueVal)],
  testCase "((True))" $
    parseString "((True))" @?= Right [SExp (Const TrueVal)],
  testCase "(((((True)))))" $
    parseString "(((((True)))))" @?= Right [SExp (Const TrueVal)],
  testCase "((((((((((True))))))))))" $
    parseString "((((((((((True))))))))))" @?= Right [SExp (Const TrueVal)],
  testCase "((((((((((((((((((((True))))))))))))))))))))" $
    parseString "((((((((((((((((((((True))))))))))))))))))))" @?= Right [SExp (Const TrueVal)]]

-- ident ‘(’ Exprz ‘)’
identExprzTest :: TestTree
identExprzTest = testGroup "ident ‘(’ Exprz ‘)’tests : " [ 
  testCase "print (True, False)" $
    parseString "print (True, False)" @?= Right [SExp (Call "print" [Const TrueVal,Const FalseVal])],
  testCase "print (123, 'haskell')" $
    parseString "print (123, 'haskell')" @?= Right [SExp (Call "print" [Const (IntVal 123),Const (StringVal "haskell")])],
  testCase "range (10)" $
    parseString "range (10)" @?= Right [SExp (Call "range" [Const (IntVal 10)])],
  testCase "range (2, n)" $
    parseString "range (2, n)" @?= Right [SExp (Call "range" [Const (IntVal 2),Var "n"])],
  testCase "range (1, 10, 3)" $
    parseString "range (1, 10, 3)" @?= Right [SExp (Call "range" [Const (IntVal 1),Const (IntVal 10),Const (IntVal 3)])],
  testCase "range (10, 1, -3)" $
    parseString "range (10, 1, -3)" @?= Right [SExp (Call "range" [Const (IntVal 10),Const (IntVal 1),Const (IntVal (-3))])]]

-- Expr Oper Expr test
operTest :: TestTree
operTest = testGroup "Expr Oper Expr tests : " [ 
  testCase "+ test" $
    parseString "1+1" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))],
  testCase "- test" $
    parseString "1-1" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 1)))],
  testCase "* test" $
    parseString "1 * 1" @?= Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 1)))],
  testCase "// test" $
    parseString "1 // 1" @?= Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 1)))],
  testCase "% test" $
    parseString "1 % 1" @?= Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 1)))],
  testCase "== test" $
    parseString "1 == 1" @?= Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 1)))],
  testCase "!= test" $
    parseString "1 != 2" @?= Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 2))))],
  testCase "< test" $
    parseString "1 < 2" @?= Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))],
  testCase "<= test" $
    parseString "1 <= 1" @?= Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 1))))],
  testCase "> test" $
    parseString "1 > 0" @?= Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 0)))],
  testCase ">= test" $
    parseString "2 >= 2" @?= Right [SExp (Not (Oper Less (Const (IntVal 2)) (Const (IntVal 2))))],
  testCase "in test" $
    parseString "x in [x, y]" @?= Right [SExp (Oper In (Var "x") (List [Var "x",Var "y"]))],
  testCase "not in test" $
    parseString "x not in [z, y]" @?= Right [SExp (Not (Oper In (Var "x") (List [Var "z",Var "y"])))]]

complexOperTest :: TestTree
complexOperTest = testGroup "Complex Expr Oper Expr tests : " [ 
  testCase "1+1-2" $
    parseString "1+1-2" @?= Right [SExp (Oper Minus (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))) (Const (IntVal 2)))],
  testCase "1*2//2" $
    parseString "1*2//2" @?= Right [SExp (Oper Div (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 2)))],
  testCase "1+1>1 test" $
    parseString "1+1>1" @?= Right [SExp (Oper Greater (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))) (Const (IntVal 1)))],
  testCase "1 % 1 == 1 test" $
    parseString "1 % 1 == 1" @?= Right [SExp (Oper Eq (Oper Mod (Const (IntVal 1)) (Const (IntVal 1))) (Const (IntVal 1)))]]

-- ‘for’ ident ‘in’ Expr test
forClauseTest :: TestTree
forClauseTest = testGroup "‘for’ ident ‘in’ Expr tests : " [
  testCase "for x in y" $
    readP_to_S forClauseP "for x in y" @?= [(CCFor "x" (Var "y"),""),(CCFor "x" (Var "y"),"")],
  testCase "no spaces: for xin y" $
    readP_to_S forClauseP "for xin y" @?= [],
  testCase "no spaces: for x iny" $
    readP_to_S forClauseP "for x iny" @?= [],
  testCase "more spaces: for\tx\tin\ty" $
    readP_to_S forClauseP "for\tx\tin\ty" @?= [(CCFor "x" (Var "y"),""),(CCFor "x" (Var "y"),"")]]

-- if’ Expr test
ifClauseTest :: TestTree
ifClauseTest = testGroup "‘if’ Expr tests : " [
  testCase "if True" $
    fst (last $ readP_to_S ifClauseP "if True") @?= CCIf (Const TrueVal),
  testCase "if 1+1=2" $
    fst (last $ readP_to_S ifClauseP "if 1+1=2") @?= CCIf (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))),
  testCase "if x!=2" $
    fst (last $ readP_to_S ifClauseP "if x!=2") @?= CCIf (Not (Oper Eq (Var "x") (Const (IntVal 2))))]

-- ‘[’ Exprz ‘]’ test
listExprzTest :: TestTree
listExprzTest = testGroup "‘[’ Exprz ‘]’ Expr tests : " [
  testCase "[ x ]" $
    parseString "[ x ]" @?= Right [SExp (List [Var "x"])],
  testCase "[x, y, 1+1]" $
    parseString "[x, y, 1+1]" @?= Right [SExp (List [Var "x",Var "y",Oper Plus (Const (IntVal 1)) (Const (IntVal 1))])],
  testCase "[x, y, 1==1]" $
    parseString "[x, y, 1==1]" @?= Right [SExp (List [Var "x",Var "y",Oper Eq (Const (IntVal 1)) (Const (IntVal 1))])],
  testCase "nest : [[x], [y]]" $
    parseString "[[x], [y]]" @?= Right [SExp (List [List [Var "x"],List [Var "y"]])],
  testCase "deep nest : [[[[x]]], [[[y]]]]" $
    parseString "[[[[x]]], [[[y]]]]" @?= Right [SExp (List [List [List [List [Var "x"]]],List [List [List [Var "y"]]]])]]

-- ‘[’ Expr ForClause Clausez ‘]’ test
lsitForClauseTest :: TestTree
lsitForClauseTest = testGroup "‘[’ Expr ForClause Clausez ‘]’ Expr tests : " [
  testCase "[x for x in y]" $
    parseString "[x for x in y]" @?= Right [SExp (Compr (Var "x") [CCFor "x" (Var "y")])],
  testCase "more spaces: [x for \tx \tin y]" $
    parseString "[x for \tx \tin y]" @?= Right [SExp (Compr (Var "x") [CCFor "x" (Var "y")])],
  testCase "no spaces: [x for xin y]" $
    (case parseString "[x for xin y]" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p)]

boaParseTest :: TestTree
boaParseTest = testGroup "Boa Parse tests : "[
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    (case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p),
  testCase "squares = [x*x for x in range(10)]" $
    parseString "squares = [x*x for x in range(10)]" @?=
      Right [SDef "squares" (Compr (Oper Times (Var "x") (Var "x")) [CCFor "x" (Call "range" [Const (IntVal 10)])])],
  testCase "print([123, [squares, print(321)]])" $
    parseString "print([123, [squares, print(321)]])" @?=
      Right [SExp (Call "print" [List [Const (IntVal 123),List [Var "squares",Call "print" [Const (IntVal 321)]]]])],
  testCase "composites = [j for i in range(2, n) for j in range(i*2, n*n, i)]" $
    parseString "composites = [j for i in range(2, n) for j in range(i*2, n*n, i)]" @?=
      Right [SDef "composites" (Compr (Var "j") [CCFor "i" (Call "range" [Const (IntVal 2),Var "n"]),CCFor "j" 
        (Call "range" [Oper Times (Var "i") (Const (IntVal 2)),Oper Times (Var "n") (Var "n"),Var "i"])])]]
