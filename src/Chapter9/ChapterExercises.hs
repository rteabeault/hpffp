module Chapter9.ChapterExercises where

import           Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

makeCapitalized :: String -> String
makeCapitalized []       = []
makeCapitalized (x : xs) = toUpper x : xs

makeCapitalized' :: String -> String
makeCapitalized' = map toUpper

headCapitalized :: String -> Char
headCapitalized s = toUpper $ head s

headCapitalized' :: String -> Char
headCapitalized' s = toUpper (head s)

headCapitalized'' :: String -> Char
headCapitalized'' = toUpper . head
