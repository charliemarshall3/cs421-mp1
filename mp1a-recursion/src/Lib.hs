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

-- don't forget to put the type declaration or you will lose points!
app = undefined

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist = undefined

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist = undefined

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip = undefined

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs = undefined

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones = undefined

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats = undefined

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib = undefined

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' = undefined

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' = undefined
