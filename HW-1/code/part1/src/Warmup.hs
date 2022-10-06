module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves (n:ns) a = moves ns (move n a) 

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) a = a
mult (Succ x) y = add (mult x y) y

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0 
nat2int (Succ a) = nat2int a + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat a = Succ (int2nat (a-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n (Node a t1 t2)
      | n == a = Node a t1 t2
      | n > a  = Node a t1 (insert n t2)
      | n < a  = Node a (insert n t1) t2

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert :: (Ord a) => a -> PTree a -> PTree a
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x (PNode y pt1 pt2)
      | z == EQ = PNode y pt1 pt2
      | z == GT = PNode y pt1 (pinsert x pt2)
      | z == LT = PNode y (pinsert x pt1) pt2
      where z = x `compare` y
