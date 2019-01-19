module Chapter7.Chapter7 where
  
myNum :: Integer
myNum = 1

myVal f g h = myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

addOneIfOdd n = case odd n of 
  True -> f n
  False -> n
  where f = \x -> x + 1
  
addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x