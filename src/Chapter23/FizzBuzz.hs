module Chapter23.FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

-- main :: IO ()
-- main =
--   mapM_ (putStrLn . fizzBuzz) [1..100]

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-- This collects the results before outputting them. However
-- we are reversing a linked list (slow) and it won't terminate on
-- an infinite list.
-- main :: IO ()
-- main =
--   mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
  let dlist =
        execState (mapM_ addResult' list) DL.empty
  -- convert back to normal list
  in DL.apply dlist []

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike
  -- cons which adds to the front put (DL.snoc xs result)
  put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzbuzzList' [1..100]

fizzbuzzList'' :: [Integer] -> DL.DList String
fizzbuzzList'' list =
  execState (mapM_ addResult'' list) DL.empty

addResult'' :: Integer -> State (DL.DList String) ()
addResult'' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList'' [1..100]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo x y
  | x == y = fizzbuzzList [x]
  | x < y && y - x == 1 = fizzbuzzList [y, x]
  | x < y = fizzbuzzList [y, y - 1 .. x]
  | otherwise = fizzbuzzList [y..x]
