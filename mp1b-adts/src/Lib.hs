--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons
-- Write a function list2cons :: [a] -> List a which converts a Haskell list into our List type. 
-- Do this recursively (not using higher-order functions).
-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list
-- Write a function cons2list :: List a -> [a] which converts our List type into the Haskell list type. 
-- Do this recursively.
-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval
-- Write a function eval :: Exp -> Integer which evaluates the integer expression represented by its input. 
-- You may use recursion and higher-order functions.
-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n) = n
eval (PlusExp exps) = sum (map eval exps)
eval (MultExp exps) = product (map eval exps)

--- ### list2cons'
-- Write a function list2cons' :: [a] -> List a which converts a Haskell list into our List type. 
-- You are required to use higher-order functions for this, no recursion.
-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' = foldr Cons Nil

--- ### BinTree
-- Write an ADT BinTree a which represents a binary tree that stores things of type a at internal nodes, and stores nothing at the leaves.
-- BinTree
data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

--- ### sumTree
-- Write a function sumTree :: Num a => BinTree a -> a which takes a BinTree a (where a is a Num) from the previous problem and sums all the elements of its nodes.
-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

--- ### SimpVal
-- Write an ADT SimpVal which represents the values that a simple programming language can have. We'll have IntVal for integers, BoolVal for booleans, StrVal for strings, and ExnVal for exceptions.
-- SimpVal
data SimpVal = IntVal Integer
              | BoolVal Bool
              | StrVal String
              | ExnVal String
  deriving (Show, Eq)

--- ### liftIntOp
-- Write a function\ liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal\ which will take an operator over integers (like (+) :: Integer -> Integer -> Integer) and turn it into an operator over SimpVals. If the inputs are not IntVal, raise an exception by returning ExnVal "not an IntVal!".
-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
