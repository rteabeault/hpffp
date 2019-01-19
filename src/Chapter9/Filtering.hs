module Chapter9.Filtering where
  
odds :: Integral xs => [xs] -> [xs]
odds xs = filter (\x -> odd x) xs

odds' :: Integral xs => [xs] -> [xs]
odds' xs = [x | x <- xs, odd x]

odds'' :: Integral xs => [xs] -> [xs]
odds'' xs = filter odd xs

odds''' :: Integral xs => [xs] -> [xs]
odds''' = filter odd

multiplesOfThree :: Integral xs => [xs] -> [xs]
multiplesOfThree xs = filter (\x -> (rem x 3) == 0) xs

numMultiplesOfThree :: Integral xs => [xs] -> Int
numMultiplesOfThree = length . multiplesOfThree

removeArticles :: String -> [String]
removeArticles s = filter (\x -> x /= "the" && x /= "a" && x /= "an") $ words s

