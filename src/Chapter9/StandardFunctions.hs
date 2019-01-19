module Chapter9.StandardFunctions where
  
-- returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- myAny returns True if a -> Bool applied to any of
-- the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = x == a || myElem a xs

-- uses any instead of recursion
myElem' :: Eq a => a -> [a] -> Bool
myElem' a xs = any (== a) xs

myReverse :: [a] -> [a]
myReverse l = acc l []
  where
    acc [] a = a
    acc (x:xs) a = acc xs (x:a)


squish :: [[a]] -> [a]
squish (x:[]) = x
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined

squishAgain :: [[a]] -> [a]
squishAgain = undefined

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined
myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined