-- Edit all the definitions with "undefined"
module RWSMonads where

import Control.Monad

type ReadData = Int
type WriteData = String  -- must be an instance of Monoid
type StateData = Double

-- Plain version of RWS monad
newtype RWSP a = RWSP {runRWSP :: ReadData -> StateData ->
                                    (a, WriteData, StateData)}

-- complete the definitions
instance Monad RWSP where
  return a = RWSP $ \r s -> (a, mempty, s)
  m >>= f = RWSP $ \r s ->  let (a, w, s') = runRWSP m r s 
                                (a', w', s'') = runRWSP (f a) r s' 
                            in (a', w <> w', s'')

-- No need to touch these
instance Functor RWSP where
  fmap = liftM
instance Applicative RWSP where
  pure = return; (<*>) = ap

-- returns current read data
askP :: RWSP ReadData
askP = RWSP (\r s -> (r, mempty, s))  -- freebie

-- runs computation with new read data
withP :: ReadData -> RWSP a -> RWSP a
withP r' m = RWSP $ \_ s -> runRWSP m r' s  

-- adds some write data to accumulator
tellP :: WriteData -> RWSP ()
tellP w = RWSP $ \ _ s -> ((),w,s)

-- returns current state data
getP :: RWSP StateData
getP = RWSP (\_ s -> (s, mempty, s))

-- overwrites the state data
putP :: StateData -> RWSP ()
putP s' = RWSP $ \_ _-> ((), mempty ,s')

-- sample computation using all features
type Answer = String
sampleP :: RWSP Answer
sampleP =
  do r1 <- askP
     r2 <- withP 5 askP
     tellP "Hello, "
     s1 <- getP
     putP (s1 + 1.0)
     tellP "world!"
     return $ "r1 = " ++ show r1 ++ ", r2 = " ++ show r2 ++ ", s1 = " ++ show s1

type Result = (Answer, WriteData, StateData)

expected :: Result
expected = ("r1 = 4, r2 = 5, s1 = 3.5", "Hello, world!", 4.5)

testP = runRWSP sampleP 4 3.5 == expected

-- Version of RWS monad with errors
type ErrorData = String
newtype RWSE a = RWSE {runRWSE :: ReadData -> StateData ->
                                    Either ErrorData (a, WriteData, StateData)}

-- Hint: here you may want to exploit that "Either ErrorData" is itself a monad
instance Monad RWSE where
  return a = RWSE $ \r s -> Right(a, mempty,s)
  m >>= f = RWSE $ \r s ->  case runRWSE m r s of
                              Left e -> Left e
                              Right (a,w,s') ->
                                case runRWSE (f a) r s' of
                                  Left e -> Left e
                                  Right (a',w',s'')->Right(a', w <> w', s'')

instance Functor RWSE where
  fmap = liftM
instance Applicative RWSE where
  pure = return; (<*>) = ap

askE :: RWSE ReadData
askE = RWSE $ \r s -> Right (r, mempty, s)

withE :: ReadData -> RWSE a -> RWSE a
withE r' m = RWSE $ \_ s -> runRWSE m r' s 

tellE :: WriteData -> RWSE ()
tellE w = RWSE $ \ _ s -> Right ((),w,s)

getE :: RWSE StateData
getE = RWSE $ \_ s -> Right (s, mempty, s)

putE :: StateData -> RWSE ()
putE s' = RWSE $ \_ _-> Right ((), mempty ,s')

throwE :: ErrorData -> RWSE a
throwE e =  RWSE $ \_ _-> Left e

sampleE :: RWSE Answer
sampleE =
  do r1 <- askE
     r2 <- withE 5 askE
     tellE "Hello, "
     s1 <- getE
     putE (s1 + 1.0)
     tellE "world!"
     return $ "r1 = " ++ show r1 ++ ", r2 = " ++ show r2 ++ ", s1 = " ++ show s1

-- sample computation that may throw an error
sampleE2 :: RWSE Answer
sampleE2 =
  do r1 <- askE
     x <- if r1 > 3 then throwE "oops" else return 6
     tellE "Blah"
     return $ "r1 = " ++ show r1 ++ ", x = " ++ show x

testE = runRWSE sampleE 4 3.5 == Right expected
testE2 = runRWSE sampleE2 4 3.5 == Left "oops"

-- Generic formulations (nothing further to add/modify)

-- The class of monads that support the core RWS operations
class Monad rws => RWSMonad rws where
  ask :: rws ReadData
  with :: ReadData -> rws a -> rws a
  tell :: WriteData -> rws ()
  get :: rws StateData
  put :: StateData -> rws ()

-- And those that additionally support throwing errors
class RWSMonad rwse => RWSEMonad rwse where
  throw :: ErrorData -> rwse a

-- RWSP is an RWS monad
instance RWSMonad RWSP where
  ask = askP; with = withP; tell = tellP; get = getP; put = putP

-- So is RWSE
instance RWSMonad RWSE where
  ask = askE; with = withE; tell = tellE; get = getE; put = putE

-- But RWSE also supports errors
instance RWSEMonad RWSE where
  throw = throwE

-- Generic sample computation, works in any RWS monad
sample :: RWSMonad rws => rws Answer
sample =
  do r1 <- ask
     r2 <- with 5 ask
     tell "Hello, "
     s1 <- get
     put (s1 + 1.0)
     tell "world!"
     return $ "r1 = " ++ show r1 ++ ", r2 = " ++ show r2 ++ ", s1 = " ++ show s1

-- Generic sample computation, works in any RWS monad supporting errors
sample2 :: RWSEMonad rwse => rwse Answer
sample2 =
  do r1 <- ask
     x <- if r1 > 3 then throw "oops" else return 6
     tell "Blah"
     return $ "r1 = " ++ show r1 ++ ", x = " ++ show x

testP' = runRWSP sample 4 3.5 == expected
testE' = runRWSE sample 4 3.5 == Right expected
testE2' = runRWSE sample2 4 3.5 == Left "oops"

allTests = [testP, testE, testE2, testP', testE', testE2']
