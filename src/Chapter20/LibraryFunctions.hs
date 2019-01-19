{-# LANGUAGE ScopedTypeVariables #-}

module Chapter20.LibraryFunctions where
  
import Data.Monoid
import Control.Applicative


-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product 

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (== a))

-- 4.
newtype Min' a = Min' { getMin' :: Maybe a }

instance Ord a => Monoid (Min' a) where
  mempty = Min' Nothing
  Min' Nothing `mappend` x = x
  x `mappend` Min' Nothing = x
  Min' a `mappend` Min' a' = Min' (min a a')

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin' . foldMap (Min' . Just)

-- 5.
newtype Max' a = Max' { getMax' :: Maybe a }

instance Ord a => Monoid (Max' a) where
  mempty = Max' Nothing
  Max' Nothing `mappend` x = x
  x `mappend` Max' Nothing = x
  Max' a `mappend` Max' a' = Max' (max a a')

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax' . foldMap (Max' . Just)

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7.
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

-- 8. Some say this is all Foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b

-- 9. Hint: use foldMap.
-- Combine the elements of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
