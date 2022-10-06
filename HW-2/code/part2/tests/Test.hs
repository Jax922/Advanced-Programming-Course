-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Boa interpreter tests : "
  [truthyTests, opTests, rangeTests, printTests,boaTests]

boaTests :: TestTree
boaTests = testGroup "Boa tests:"
  [ testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
    testCase "print StringVal test" $
    execute [SExp (Call "print" [Const (StringVal "CPR")])]
      @?= (["CPR"], Nothing),
    testCase "print ListVal test" $
    execute [SExp (Call "print" [Const (ListVal [(IntVal 1), (IntVal 2), (IntVal 3)])])]
      @?= (["[1, 2, 3]"], Nothing),
    testCase "print IntVal test" $
    execute [SExp (Call "print" [Const (IntVal 24)])]
      @?= (["24"], Nothing),
    testCase "plus op test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 12))
                                           (Const (IntVal 12))])]
      @?= (["24"], Nothing),
    testCase "minus op test" $
    execute [SExp (Call "print" [Oper Minus (Const (IntVal 12))
                                           (Const (IntVal 12))])]
      @?= (["0"], Nothing),
    testCase "times op test" $
    execute [SExp (Call "print" [Oper Times (Const (IntVal 12))
                                           (Const (IntVal 12))])]
      @?= (["144"], Nothing),
    testCase "div op test" $
    execute [SExp (Call "print" [Oper Div (Const (IntVal 12))
                                           (Const (IntVal 12))])]
      @?= (["1"], Nothing),
    testCase "mod op test" $
    execute [SExp (Call "print" [Oper Mod (Const (IntVal 12))
                                           (Const (IntVal 12))])]
      @?= (["0"], Nothing),
    testCase "eq op test" $
    execute [SExp (Call "print" [Oper Eq (Const (IntVal 12))
                                           (Const (IntVal 12))])]
      @?= (["True"], Nothing),
    testCase "less op test" $
    execute [SExp (Call "print" [Oper Less (Const (IntVal 11))
                                           (Const (IntVal 12))])]
      @?= (["True"], Nothing),
    testCase "grater op test" $
    execute [SExp (Call "print" [Oper Greater (Const (IntVal 12))
                                           (Const (IntVal 11))])]
      @?= (["True"], Nothing),
    testCase "range(10) test" $
    execute [
      SExp (Call "print" [(Call "range" [Const (IntVal 10)])])
    ]
      @?= (["[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"], Nothing),
    testCase "range(1,10) test" $
    execute [
      SExp (Call "print" [(Call "range" [(Const (IntVal 1)), 
                                          (Const (IntVal 10))])])
    ]
      @?= (["[1, 2, 3, 4, 5, 6, 7, 8, 9]"], Nothing),
    testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing)  
  ]

-- truthy function unit tests
truthyTests :: TestTree
truthyTests = testGroup "truthy function tests:"
  [
    testCase "truthy NoneVal" $
    truthy NoneVal @?= False,
    testCase "truthy FalseVal" $
    truthy FalseVal @?= False,
    testCase "truthy IntVal 0" $
    truthy (IntVal 0) @?= False,
    testCase "truthy StringVal ''" $
    truthy (StringVal "") @?= False,
    testCase "truthy ListVal []" $
    truthy (ListVal []) @?= False,
    testCase "truthy IntVal 24" $
    truthy (IntVal 24 ) @?= True,
    testCase "truthy StringVal 'CPR'" $
    truthy (StringVal "CPR") @?= True,
    testCase "truthy ListVal [IntVal 1]" $
    truthy (ListVal [IntVal 1]) @?= True
  ]

-- operate function unit tests
-- Op = Plus | Minus | Times | Div | Mod | Eq | Less | Greater | In
opTests :: TestTree
opTests = testGroup "operate function tests:"
  [
    testCase "Plus test" $
    operate Plus (IntVal 2) (IntVal 2) @?= (Right (IntVal 4)),
    testCase "Minus test" $
    operate Minus (IntVal 2) (IntVal 2) @?= (Right (IntVal 0)),
    testCase "Times test" $
    operate Times (IntVal 2) (IntVal 2) @?= (Right (IntVal 4)),
    testCase "Div test" $
    operate Div (IntVal 2) (IntVal 2) @?= (Right (IntVal 1)),
    testCase "Div error test" $
    operate Div (IntVal 2) (IntVal 0) @?= (Left "err:can't be zero"),
    testCase "Mod test" $
    operate Mod (IntVal 2) (IntVal 2) @?= (Right (IntVal 0)),
    testCase "Mod error test" $
    operate Mod (IntVal 2) (IntVal 0) @?= (Left "err:can't be zero"),
    testCase "Eq test" $
    operate Eq (IntVal 2) (IntVal 2) @?= (Right TrueVal),
    testCase "Less test" $
    operate Less (IntVal 1) (IntVal 2) @?= (Right TrueVal),
    testCase "Greater test" $
    operate Greater (IntVal 2) (IntVal 1) @?= (Right TrueVal),
    testCase "In test" $
    operate In (IntVal 2) (ListVal [IntVal 2]) @?= (Right TrueVal)
  ]

-- range function unit tests
rangeTests :: TestTree
rangeTests = testGroup "range function tests:"
  [
    testCase "rangeFun(2)" $
    rangeFun [(IntVal 2)] @?= (Right [(IntVal 0), (IntVal 1)]),
    testCase "rangeFun(2,4)" $
    rangeFun [(IntVal 2), (IntVal 4)] @?= (Right [(IntVal 2), (IntVal 3)]),
    testCase "rangeFun(1,4,2)" $
    rangeFun [(IntVal 1), (IntVal 4), (IntVal 2)] @?=
       (Right [(IntVal 1), (IntVal 3)]),
    testCase "rangeFun(1,4,0)" $
    rangeFun [(IntVal 1), (IntVal 4), (IntVal 0)] @?=
       (Left (EBadArg "range function need v3 can not be 0")),
    testCase "rangeFun(4,1,-2)" $
    rangeFun [(IntVal 4), (IntVal 1), (IntVal (-2))] @?=
       (Right [(IntVal 4), (IntVal 2)])
  ]


-- print function unit tests
printTests :: TestTree
printTests = testGroup "print function tests:"
  [
    testCase "printFun NoneVal" $
    printFun NoneVal @?= "None",
    testCase "printFun TrueVal" $
    printFun TrueVal @?= "True",
    testCase "printFun FalseVal" $
    printFun FalseVal @?= "False",
    testCase "printFun (IntVal 24)" $
    printFun (IntVal 24) @?= "24",
    testCase "printFun (StringVal 'CPR')" $
    printFun (StringVal "CPR") @?= "CPR",
    testCase "printFun ListVal [(IntVal 1), (IntVal 2), (IntVal 3)]" $
    printFun (ListVal [(IntVal 1), (IntVal 2), (IntVal 3)]) @?= "[1, 2, 3]"
  ]

