module Chapter7.ChapterExercises where
  
f :: Char -> String
f = undefined

g :: String -> [String]
g = undefined

f' :: Ord a => a -> a -> Bool
f' = undefined

f'' :: a -> a
f'' x = x

tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10
        
tensDigit' :: Integral a => a -> a 
tensDigit' x = d
  where xLast = x `div` 10
        d     = snd (xLast `divMod` 10)
        
hundresDigit x = d
  where xLast = x `div` 100
        d     = snd (xLast `divMod` 10)

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True -> x
  False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a 
foldBool3 x y True = x 
foldBool3 x y False = y

g' :: (a -> b) -> (a, c) -> (b, c) 
g' f x = (f (fst x), (snd x))

roundTrip :: (Show a, Read b) => a -> b 
roundTrip = read . show

main = do
  print (roundTrip 4 :: Int) 
  print (id 4)