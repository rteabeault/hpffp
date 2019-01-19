module Chapter16.FunctorQuickCheck where
  
import           Test.QuickCheck
import           Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) =>
  (a -> b) ->
  (b -> c) ->
  f a ->
  Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) =>
  f a ->
  Fun a b ->
  Fun b c ->
  Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap g (fmap f x)) == (fmap (g . f) x)

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a  b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

-- 8 No. Trivial is not a higher-kinded type


type IntToInt = Fun Int Int
type ListFunctorComposition a = [a] -> Fun a a -> Fun a a -> Bool
type IdentityFunctorComposition a = Identity a -> Fun a a -> Fun a a -> Bool
type PairFunctorComposition a = Pair a -> Fun a a -> Fun a a -> Bool
type TwoFunctorComposition a b = Two a b -> Fun b b -> Fun b b -> Bool
type ThreeFunctorComposition a b c = Three a b c -> Fun c c -> Fun c c -> Bool
type Three'FunctorComposition a b = Three' a b -> Fun b b -> Fun b b -> Bool
type FourFunctorComposition a b c d = Four a b c d -> Fun d d -> Fun d d -> Bool
type Four'FunctorComposition a b = Four' a b -> Fun b b -> Fun b b -> Bool

main :: IO ()
main = do
  putStrLn "Checking Functor Instances..."

  putStrLn "Checking [Int] Functor Identity"
  quickCheck (functorIdentity :: [Int] -> Bool)
  putStrLn "Checking [Int] Functor Composition"
  quickCheck (functorCompose' :: ListFunctorComposition Int)

  putStrLn "Checking Identity Int Functor Identity"
  quickCheck (functorIdentity :: Identity Int -> Bool)
  putStrLn "Checking Identity Int Functor Composition"
  quickCheck (functorCompose' :: IdentityFunctorComposition Int)

  putStrLn "Checking Pair Int Functor Identity"
  quickCheck (functorIdentity :: Pair Int -> Bool)
  putStrLn "Checking Pair Int Functor Composition"
  quickCheck (functorCompose' :: PairFunctorComposition Int)

  putStrLn "Checking Two Int Int Functor Identity"
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  putStrLn "Checking Two Int Int Functor Composition"
  quickCheck (functorCompose' :: TwoFunctorComposition Int Int)

  putStrLn "Checking Three Int Int Int Functor Identity"
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  putStrLn "Checking Three Int Int Int Functor Composition"
  quickCheck (functorCompose' :: ThreeFunctorComposition Int Int Int)

  putStrLn "Checking Three' Int Int Functor Identity"
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  putStrLn "Checking Three Int Int Functor Composition"
  quickCheck (functorCompose' :: Three'FunctorComposition Int Int)

  putStrLn "Checking Four Int Int Int Int Functor Identity"
  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  putStrLn "Checking Four Int Int Int Int Functor Composition"
  quickCheck (functorCompose' :: FourFunctorComposition Int Int Int Int)

  putStrLn "Checking Four' Int Int Functor Identity"
  quickCheck (functorIdentity :: Four' Int Int -> Bool)
  putStrLn "Checking Four' Int Int Functor Composition"
  quickCheck (functorCompose' :: Four'FunctorComposition Int Int)
