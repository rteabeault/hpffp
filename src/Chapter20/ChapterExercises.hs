module Chapter20.ChapterExercises where

import Data.Monoid
    
-- Write Foldable instances for the following datatypes.
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b

-- 1.
data Constant a b = Constant b

instance Foldable (Constant a) where
 foldMap f (Constant b) = f b
 foldr f x (Constant b) = f b x

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b
  foldr f x (Two _ b) = f b x

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f x (Three _ _ c) = f c x

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''


-- Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if (f a) then (pure a) else mempty)
