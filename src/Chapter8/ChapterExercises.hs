module Chapter8.ChapterExercises where
  
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

sumFrom :: (Eq a, Num a) => a -> a
sumFrom 0 = 0
sumFrom x = x + sumFrom(x - 1)