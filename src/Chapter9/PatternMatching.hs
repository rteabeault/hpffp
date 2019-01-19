module Chapter9.PatternMatching where

myHead :: [a] -> a  
myHead (x : _) = x

myHead' :: [a] -> Maybe a
myHead' [] = Nothing  
myHead' (x : _) = Just x


