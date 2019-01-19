{-# LANGUAGE FlexibleContexts #-}

module Chapter21.ChapterExercises where
  
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write a Traversable instance for the datatype provided, filling in any
-- required superclasses. Use QuickCheck to validate your instances.

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f b (Identity a) = f a b

-- traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> (f a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty
  foldr _ b _ = b

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Eq a => EqProp (Constant a b) where
  x =-= x' = (getConstant x) `eq` (getConstant x') 

instance Arbitrary a => Arbitrary (Constant a b)  where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Yep a) = f a

  foldr f b Nada = b
  foldr f b (Yep a) = f a b

instance Traversable Optional where
  traverse _ (Nada) = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, fmap Yep arbitrary]

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = (f x) `mappend` foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [ (1, return Nil)
              , (3, return $ Cons x xs)]

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> f b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

-- Big
-- When you have more than one value of type 𝑏, you’ll want to use Monoid and Applicative
-- for the Foldable and Traversable instances respectively.

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b')= Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = (Big a) <$> f b <*> f b'

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

-- Bigger
-- Same as for Big.

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = (f b) <> (f b') <> (f b'')

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = (Bigger a) <$> f b <*> f b' <*> f b''

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    b'' <- arbitrary
    return $ Bigger a b b' b''

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

instance (Functor n
         , Arbitrary (n a)
         , Arbitrary a )
         => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

-- Tree

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> (traverse f l) <*> f a <*> (traverse f r)

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [ (1, return $ Empty)
              , (4, return $ Leaf a)
              , (4, return $ Node l a r) ]

main = do
  putStrLn "Identity"
  quickBatch $ traversable $ (undefined :: Identity (Int, Int, [Int]))
  putStrLn "\nConstant"
  quickBatch $ traversable $ (undefined :: Constant Int (Int, Int, [Int]))
  putStrLn "\nOptional"
  quickBatch $ traversable $ (undefined :: Optional (Int, Int, [Int]))
  putStrLn "\nList"
  quickBatch $ traversable $ (undefined :: List (Int, Int, [Int]))
  putStrLn "\nThree"
  quickBatch $ traversable $ (undefined :: Three Int Int (Int, Int, [Int]))
  putStrLn "\nPair"
  quickBatch $ traversable $ (undefined :: Pair Int (Int, Int, [Int]))
  putStrLn "\nBig"
  quickBatch $ traversable $ (undefined :: Big Int (Int, Int, [Int]))
  putStrLn "\nBigger"
  quickBatch $ traversable $ (undefined :: Bigger Int (Int, Int, [Int]))
  putStrLn "\nS"
  quickBatch $ traversable $ (undefined :: S Maybe (Int, Int, [Int]))
  putStrLn "\nTree"
  quickBatch $ traversable $ (undefined :: Tree (Int, Int, [Int]))
