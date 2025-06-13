--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n _ | n <= 0 = []
mytake n (x:xs) = x : mytake (n-1) xs

--- ### mydrop
-- Write a function mydrop :: Int -> [a] -> [a] which drops the first n elements of a list, 
-- or the whole list if there are not n elements. It should behave exactly like the Haskell built-in
-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n xs | n <= 0 = xs
mydrop n (_:xs) = mydrop (n-1) xs

--- ### rev
-- Write a function rev :: [a] -> [a] which reverses the input list. 
-- To get credit, your solution must run in linear time. 
-- If you use the (++) list append operator, chances are your solution is running in quadratic time. 
-- This function should behave exactly like the Haskell built-in reverse :: [a] -> [a].
-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = reverser xs []
  where
    reverser [] acc = acc
    reverser (x:xs) acc = reverser xs (x:acc)

--- ### app
-- Write a function app :: [a] -> [a] -> [a] which appends two lists. 
-- This function should behave like the Haskell built-in (++) :: [a] -> [a] -> [a], 
-- and should run in linear time (in the size of the first list).
-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] ys = ys
app (x:xs) ys = x : app xs ys

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
-- Write a function inclist :: Num a => [a] -> [a] which adds 1 to each element of the input list.
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x + 1) : inclist xs

--- ### sumlist
-- Write a function sumlist :: Num a => [a] -> a which adds all the elements of the input list.
-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip
-- Write a function myzip :: [a] -> [b] -> [(a,b)] which zips up the elements of two lists. 
-- The resulting list should be the same length as the shorter of the two input lists.
-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs
-- Write a function addpairs :: (Num a) => [a] -> [a] -> [a] which zips up two lists using the addition operator (+). 
-- You must use your function myzip.
-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = addHelper (myzip xs ys) 
  where
      addHelper [] = []
      addHelper ((x, y):xs) = x+y: addHelper xs

--- ### ones
-- Write a (constant) function ones :: [Integer] which produces an infinite list of the integer 1.
-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones

--- ### nats
-- Write a (constant) function nats :: [Integer] which produces an infinite list of all the natural numbers starting at 0. 
-- It is OK for this function if you do not use recursion.
-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = natsStart 0
  where
    natsStart n = n:natsStart (n+1)

--- ### fib
-- Write a (constant) function fib :: [Integer] which produces an infinite list of the Fibonacci series starting with numbers 0 and 1. 
-- You can (and should) use your addpairs function here. This is the one place in the assignment where it really makes sense to use tail :: [a] -> [a].
-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0:1:addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add
-- Write a function add :: Ord a => a -> [a] -> [a] which will add an element to the set. 
-- Remember that it must add it in the correct place to ensure that the list remains sorted. 
-- It should run in linear time (in the size of the list).
-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys)
  | x < y     = x : y : ys
  | x == y    = y : ys
  | otherwise = y : add x ys

--- ### union
-- Write a function union :: Ord a => [a] -> [a] -> [a] which unions two input sets (Haskell lists). 
-- This should look similar to the "merge" step of merge-sort, and should run in linear time (in the added sizes of the input sets). 
-- You may use the add function defined above, but if you do it probably will not run in linear time, so it's probably better not to.
-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys)
  | x == y    = x: union xs ys
  | x < y     = x: union xs (y:ys)
  | otherwise = y: union (x:xs) ys

--- ### intersect
-- Write a function intersect :: Ord a => [a] -> [a] -> [a] which intersects two input sets. 
-- This should run in linear time (in the size of the input sets).
-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect xs [] = []
intersect [] ys = []
intersect (x:xs) (y:ys)
  | x == y    = x: intersect xs ys
  | x < y     = intersect xs (y:ys)
  | otherwise = intersect (x:xs) ys 

--- ### powerset
-- Write a function powerset :: Ord a => [a] -> [[a]] which calculates the powerset of the input set. 
-- Because the output is also a set, it must preserve our set properties, including that there are no duplicate elements and the elements are lexicographically sorted. 
-- Using the functions union and add that you've already defined is useful here.
-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) =
  let ps = powerset xs
  in union ps (addElement x ps)

addElement :: Ord a => a -> [[a]] -> [[a]]
addElement _ [] = []
addElement x (ys:yss) = add x ys : addElement x yss


--- Higher Order Functions
--- ----------------------

--- ### inclist'
-- Write a function inclist' :: Num a => [a] -> [a] which increments each element of an input list.
-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1)

--- ### sumlist'
-- Write a function sumlist' :: (Num a) => [a] -> a which adds all the elements of the input list.
-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' = P.foldr (+) 0
