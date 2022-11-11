-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the AutoProg APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Defs
import Parser
import Resolver
import Coder

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testGroup "Parser" [
    testCase "...Type" $
      parseStringType "a -> a" @?= Right pt0,
    testCase "...TDeclz" $
      parseStringTDeclz "type T a = a -> a" @?= Right [td0]
  ],
  testGroup "Resolver" [
    testCase "resolve" $
      resolve tce0 (\x -> return $ STVar (x++"'")) pt0 @?= Right st0,
    testCase "declare" $
      do tce <- declare [td0]
         tf <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T"
         tf [STVar "a'"]
      @?= Right st0
  ],
  testGroup "Coder" [
    testCase "pick" $
      do n <- pick [0,3]
         if n > 0 then return n
         else do m <- pick [4,0]
                 if m > 0 then return m else pick []
      @?= tr0,
    testCase "solutions" $
      solutions tr0 10 Nothing @?= [3,4],
    testCase "produce" $
      do e <- dfs (produce [] st0)
         return $ case e of
                    Lam x (Var x') | x' == x -> e0
                    _ -> e 
      @?= [e0]
    ]]
 where pt0 = PTApp "(->)" [PTVar "a", PTVar "a"]
       td0 = TDSyn ("T", ["a"]) pt0
       st0 = STArrow (STVar "a'") (STVar "a'")
       tr0 = Choice [Choice [Found 4, Choice []], Found 3]
       dfs (Found a) = [a]
       dfs (Choice ts) = concatMap dfs ts
       e0 = Lam "X" (Var "X")
