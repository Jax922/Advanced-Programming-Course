-- Very rudimentary test of Arithmetic. Feel free to replace completely
import Definitions
import Arithmetic

import Data.List (intercalate)
import Data.Either (isRight)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests :"
  [testShowExp, testEvalSimple, testEvalFull, testEvalErr]

testShowExp :: TestTree
testShowExp = testGroup "showExp Tests : "
  [
    testCase "Cst 2" $
     showExp (Cst 2) @?= "2",
    testCase "Mul (Cst 2) (Add (Cst 3) (Cst 4))" $
     showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) @?= "(2*(3+4))",
    testCase "Add (Mul (Cst 2) (Cst 3)) (Cst 4)" $
     showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) @?= "((2*3)+4)",
    testCase "Add (Div (Cst 2) (Cst 3)) (Cst 4)" $
     showExp (Add (Div (Cst 2) (Cst 3)) (Cst 4)) @?= "((2`div`3)+4)"
  ]

testEvalSimple :: TestTree
testEvalSimple = testGroup "evalSimple Tests : "
  [
    testCase "Add (Cst 2) (Cst 2)" $
     evalSimple (Add (Cst 2) (Cst 2)) @?= 4,
    testCase "Mul (Cst 2) (Add (Cst 3) (Cst 4))" $
     evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) @?= 14,
    testCase "Add (Mul (Cst 2) (Cst 3)) (Cst 4)" $
     evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) @?= 10,
    testCase "Add (Div (Cst 2) (Cst 3)) (Cst 4)" $
     evalSimple (Add (Div (Cst 2) (Cst 3)) (Cst 4)) @?= 4,
    testCase "Mul (Add (Cst 3) (Cst 4)) (Add (Div (Cst 2) (Cst 3)) (Cst 4))" $
     evalSimple (Mul (Add (Cst 3) (Cst 4)) (Add (Div (Cst 2) (Cst 3)) (Cst 4))) @?= 28
  ]


testEvalFull :: TestTree
testEvalFull = testGroup "evalFull Tests : "
  [
    testCase "Let 'var'  (Div (Cst 4) (Cst 2))  (Cst 5) initEnv" $
      evalFull (Let "var" (Div (Cst 4) (Cst 2)) (Cst 5)) initEnv @?= 5,
    testCase "Let 'var'  (Div (Cst 4) (Cst 0))  (Cst 5) initEnv" $
      evalFull (Let "var" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv @?= 5,
    testCase "Sum 'xx'  (Cst 1)  (Add (Cst 2) (Cst 2))(Mul (Var 'xx') (Var 'xx')) initEnv" $
      evalFull (Sum "xx"  (Cst 1)  (Add (Cst 2) (Cst 2))(Mul (Var "xx") (Var "xx"))) initEnv @?= 30,
    testCase "If {test = Sub (Cst 2) (Cst 2),yes = Div (Cst 3) (Cst 0),no = Cst 5} initEnv" $
      evalFull (If {test = Sub (Cst 2) (Cst 2),yes = Div (Cst 3) (Cst 0),no = Cst 5}) initEnv @?= 5
  ]

testEvalErr :: TestTree
testEvalErr = testGroup "evalErr Tests : "
  [
    testCase "Let 'var'  (Div (Cst 4) (Cst 2))  (Cst 5) initEnv" $
      evalErr (Let "var" (Div (Cst 4) (Cst 2)) (Cst 5)) initEnv @?= (Right 5),
    testCase "Div (Cst 4) (Cst 0) initEnv" $
      assertEqual "This will return Left EDivZero" False (isRight (evalErr (Div (Cst 4) (Cst 0)) initEnv))
  ]
   


  


-- tests :: [(String, Bool)]
-- tests = [test1, test2, test3] where
--   test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
--   test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
--   test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))

-- main :: IO ()
-- main =
--   let failed = [name | (name, ok) <- tests, not ok]
--   in case failed of
--        [] -> do putStrLn "All tests passed!"
--                 exitSuccess
--        _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
--                exitFailure



