{-# OPTIONS_GHC -fwarn-tabs #-}
-- Haskell is space sensitive

{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
-- Turn on warnings

module Examples where

import Prelude hiding (map)

import Test.QuickCheck

-- Load this file into GHCi and type `isThisWorking` at the prompt. GHCi will
-- tell you whether it's working!
--
-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can load this file by starting GHCi at the UNIX prompt with the command
-- `stack ghci Examples.hs`.
--
-- You can also load the file into GHCi after starting it by typing `:load
-- Examples.hs` once GHCi has started.
--
-- You can reload a file in GHCi after making changes by typing `:reload`.

isThisWorking :: String
isThisWorking = "Yes"

--
-- List examples
--

-- Return the nth element of a list, counting from 0.
nth :: Int -> [a] -> a                  -- Type tells  us what the function does.
nth _ [] = error "Empty list"           -- Matches anything but doesn't create a binding.
nth 0 (x:_) = x                         -- 0th item of x cons... is x.
nth n xs = nth (n-1) (tail xs)  

{-}
cons :: a -> [a] -> [a]                 -- Any a and a list of a returns a list of a, thats what cons is.
conds =(:)
-}

-- We use generators in this test. See Section 5.1 of PIH. [1..5] is the list
-- containing the numbers 1 through 5, inclusive.
prop_nth_1_through_5 :: Bool
prop_nth_1_through_5 = nth 2 [1..5] == 3

-- Map a function over the elements of a list
map :: (a -> b) -> [a] -> [b]           -- Takes a function a and applies it to b to create a list of b. a and b types have to remain consistent.
map _ [] = []                           -- This is the predicate testing for the empty list.
map f (x:xs) = f x : map f xs           -- Applied function cons the rest of the list.

prop_map_inc :: Bool
prop_map_inc = map (\x -> x + 1) [1,2,3,4,5] == [2,3,4,5,6]

-- Append two lists
append :: [a] -> [a] -> [a]             -- Implies that lists must have the same type, and we smush them together.
append []     ys = ys                   -- If the first list is empty, return the second list.
append (x:xs) ys = x : append xs ys

-- Try
foo :: [a] -> [b] -> [c]
foo _ _ = []                            -- For any list of a and b, return an empty list of c.
                                        -- This is the only way to terminate the function.

-- This is a general property-based test! Very powerful way to test stuff.
prop_append_length :: [Int] -> [Int] -> Bool
prop_append_length xs ys = length (append xs ys) == length xs + length ys

-- Double every element
doubleEveryElement :: [a] -> [a]
doubleEveryElement = error "doubleEveryElement unimplemented"

-- Another property-bases test
prop_double_length :: [Int] -> Bool
prop_double_length xs = length (doubleEveryElement xs) == 2 * length xs

--
-- Partial application
--

-- Increment an integer
inc :: Int -> Int
inc x = x + 1
-- inc = \x -> x + 1                -- "\" is the lambda function.
-- inc = (+ 1_)

{-} Scheme implementation of explicit lambda function
(define (inc x) 
    (lambda (x) (+ x 1)))
-}

prop_inc :: Int -> Bool
prop_inc x = inc x == x + 1

-- A function that squares all elements in a list.
squareAll :: [Int] -> [Int]
squareAll = map (\x -> x * x)
-- squareAll xs = map (\x -> x * x) xs          -- To fully apply the function, and not partially apply it.

-- We use list comprehensions in this test. See Section 5.1 of PIH. [1..5] is
-- the list containing the numbers 1 through 5, inclusive.
prop_squareAll :: Bool
prop_squareAll = squareAll [1..5] == [x*x | x <- [1..5]]

-- To convert x from Celsius to Fahrenheit, compute x * 9/5 + 32
celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit = error "celsiusToFahrenheit unimplemented"

prop_celsius0 :: Bool
prop_celsius0 = celsiusToFahrenheit 0 == 32

prop_celsius100 :: Bool
prop_celsius100 = celsiusToFahrenheit 100 == 212

-- Return every third element of a list
everyThird :: [a] -> [a]
everyThird [_]          = []
-- everyThird (_:[])    = []
everyThird [_, _]       = []
everyThird [_:_:x:xs]   = x : everyThird xs     -- We expand just enough to get the third element and the tail. 

prop_everyThird :: Bool
prop_everyThird = everyThird [1, 2, 3, 4, 5, 6, 7] == [3, 6]

curry :: ((a, b) -> c) -> (a -> b -> c)
--curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)                          -- Type here forces us to implement curry this way.

-- | This main function runs all HSpec tests
--
-- `main` is executed when you compile and run this program. We'll cover `do`
-- notation later in the course. You can add quickCheck tests here.
--
-- To run the tests, type `main` at the GHCi prompt.
main :: IO ()
main = do
    quickCheck prop_nth_1_through_5
    quickCheck prop_map_inc
    quickCheck prop_append_length
    quickCheck prop_double_length
    quickCheck prop_inc
    quickCheck prop_squareAll
    quickCheck prop_celsius0
    quickCheck prop_celsius100
    quickCheck prop_everyThird
