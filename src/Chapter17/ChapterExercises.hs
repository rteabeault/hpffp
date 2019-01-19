module Chapter17.ChapterExercises where
  
import Data.Monoid
import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1. -- Type []
pureList :: a -> [a]
pureList = pure

applicativeList ::  [(a -> b)] -> [a] -> [b]
applicativeList = (<*>)

-- 2. -- Type IO
pureIO :: a -> IO a
pureIO = pure

applicativeIO :: IO (a -> b) -> IO a -> IO b
applicativeIO = (<*>)

-- 3. -- Type (,) a
pureTuple :: Monoid a => a -> (a, a)
pureTuple = pure

applicativeTuple :: Monoid a => (a, (a -> b)) -> (a, a) -> (a, b)
applicativeTuple = (<*>)

-- 4. -- Type (->) e
pureFunction :: a -> e -> a
pureFunction = pure

applicativeFunction :: (e -> a -> b) -> (e -> a) -> (e -> b)
applicativeFunction = (<*>)

-- Applicative instances
-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')
  
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' b = Two (a <> a')(f b) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a  b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' c = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f f' <*> Three' a' b b' = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

--6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a b c f <*> Four' a' b' c' d = Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,) 

type S = String
type I = Int

main :: IO ()
main = do
  putStrLn "\n Pair Applicative"
  quickBatch (applicative (undefined :: Pair (I, I, I)))
  putStrLn "\n Two Applicative"
  quickBatch (applicative (undefined :: Two (S, S, S) (I, I, I)))
  putStrLn "\n Three Applicative"
  quickBatch (applicative (undefined :: Three (S, S, S) (S, S, S)(I, I, I)))
  putStrLn "\n Three' Applicative"
  quickBatch (applicative (undefined :: Three' (S, S, S) (I, I, I)))
  putStrLn "\n Four Applicative"
  quickBatch (applicative (undefined :: Four (S, S, S) (S, S, S) (S, S, S) (I, I, I)))
  putStrLn "\n Four' Applicative"
  quickBatch (applicative (undefined :: Four' (S, S, S) (I, I, I)))
