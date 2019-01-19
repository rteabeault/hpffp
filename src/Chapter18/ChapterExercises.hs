module Chapter18.ChapterExercises where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Monad

import qualified Data.Map as Map

-- 1.
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

-- m a -> (a -> m b) -> m b
instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- 2.
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Right' e <*> _ = Right' e
  Left' f <*> r = fmap f r

instance Monad (PhhhbbtttEither b) where
  return = pure
  Right' e >>= _ = Right' e
  Left' a >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [Left' a, Right' b]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a) 
  
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 4.
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (fmap f xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
   h <- arbitrary
   t <- arbitrary 
   frequency [(3, return $ Cons h t), (1, return Nil)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

-- Write the following functions using the methods provided by Monad and Functor
-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

--3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma
-- This can also be done with flip
-- a = flip <*>

--5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = do
  x' <- f x
  fmap ((:) x') (meh xs f)

--6.
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = (flip meh) id xs

--
type I = Int

main :: IO ()
main = do
  quickBatch (monad (undefined :: Nope (I, I, I)))
  quickBatch (monad (undefined :: PhhhbbtttEither (I, I, I) (I, I, I)))
  quickBatch (monad (undefined :: Identity (I, I, I)))
  quickBatch (monad (undefined :: List (I, I, I)))
