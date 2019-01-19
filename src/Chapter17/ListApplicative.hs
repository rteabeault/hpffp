module Chapter17.ListApplicative where
  
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

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

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold mappend Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys' where
    xs' = let (ZipList' l) = xs
     in take' 3000 l
    ys' = let (ZipList' l) = ys
     in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' Nil <*> _ = ZipList' Nil
  ZipList' (Cons f Nil) <*> ZipList' (Cons x xs) = ZipList' $ Cons (f x) (pure f <*> xs)
  ZipList' (Cons f fs) <*> ZipList' (Cons x Nil) = ZipList' $ Cons (f x) (fs <*> pure x)
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' $ Cons (f x) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
   h <- arbitrary
   t <- arbitrary 
   frequency [(3, return $ Cons h t), (1, return Nil)]

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    xs <- arbitrary
    return $ ZipList' xs 

main :: IO ()
main = do
  putStrLn "Testing ZipList' applicative laws"
  quickBatch (applicative $ ZipList' (Cons (undefined :: (Int, Bool, Int)) Nil))
