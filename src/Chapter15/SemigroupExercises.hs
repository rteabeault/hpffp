module Chapter15.SemigroupExercises where

import           Data.Semigroup
import           Test.QuickCheck     hiding (Failure, Success)
import           Text.Show.Functions ()

-- Association
semigroupAssociativity :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssociativity a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type Associativity x = x -> x -> x -> Bool
type FunctionAssociativity x a = x -> x -> x -> a -> Bool

-- Identity
type LeftIdentity x = x -> Bool
type RightIdentity x = x -> Bool
type FunctionIdentity x a = x -> a -> Bool

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => LeftIdentity m
monoidLeftIdentity m = mempty <> m == m

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => RightIdentity m 
monoidRightIdentity m = m <> mempty == m

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  Identity a1 <> Identity a2 = Identity $ a1 <> a2

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a =>
  Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

 -- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a1 b1 c1 <> Three a2 b2 c2 = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a1 b1 c1 d1 <> Four a2 b2 c2 d2 = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

-- Or
data Or a b
  = Fst a

  | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  Fst _ <> z = z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ map return [Fst a, Snd b]

-- Combine
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Show (Combine a b) where
  show (Combine f) = show f

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

combineAssociativity
  :: (Eq b, Semigroup b, Show b)
  => FunctionAssociativity (Combine a b) a
combineAssociativity f g h a =
  unCombine ((f <> g) <> h ) a == unCombine (f <> (g <> h)) a

combineLeftIdentity
  :: (Eq b, Show b, Semigroup b, Monoid b)
  => FunctionIdentity (Combine a b) a
combineLeftIdentity f a = unCombine (mempty <> f) a == unCombine f a

combineRightIdentity
  :: (Eq b, Show b, Semigroup b, Monoid b)
  => FunctionIdentity (Combine a b) a
combineRightIdentity f a = unCombine (f <> mempty) a == unCombine f a

-- Comp
newtype Comp a =
  Comp { unComp :: a -> a }

instance Show (Comp a) where
  show (Comp f) = show f

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f <> g)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

compAssociativity
  :: (Eq a, Semigroup a, Show a)
  => FunctionAssociativity (Comp a) a
compAssociativity f g h a =
  unComp ((f <> g) <> h) a == unComp (f <> (g <> h)) a

compLeftIdentity :: (Eq a, Show a, Semigroup a, Monoid a) => FunctionIdentity (Comp a) a
compLeftIdentity f a = unComp (mempty <> f) a == unComp f a

compRightIdentity :: (Eq a, Show a, Semigroup a, Monoid a) => FunctionIdentity (Comp a) a
compRightIdentity f a = unComp (f <> mempty) a == unComp f a 

-- Validation
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Validation a b) where
  Success a <> _ = Success a
  _ <> Success a = Success a
  Failure a1 <> Failure a2 = Failure (a1 <> a2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ map return [Failure a, Success b]

-- Mem
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Show (Mem s a) where
  show (Mem f) = show f

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \s ->
    let (a, s')   = f s
        (a', s'') = g s'
    in (a <> a', s'')

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

memAssociativity
  :: (Eq a, Eq s, Monoid a, Semigroup a)
  => FunctionAssociativity (Mem s a) s
memAssociativity f g h s = runMem ((f <> g) <> h) s == runMem (f <> (g <> h)) s

memLeftIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => FunctionIdentity (Mem s a) s
memLeftIdentity f s = runMem (mempty <> f) s == runMem f s

memRightIdentity :: (Eq s, Eq a, Semigroup a, Monoid a) => FunctionIdentity (Mem s a) s
memRightIdentity f s = runMem (f <> mempty) s == runMem f s 

main :: IO ()
main = do
  quickCheck (semigroupAssociativity :: Associativity Trivial)
  quickCheck (monoidLeftIdentity :: LeftIdentity Trivial)
  quickCheck (monoidRightIdentity :: RightIdentity Trivial)
  quickCheck (semigroupAssociativity :: Associativity (Identity String))
  quickCheck (monoidLeftIdentity :: LeftIdentity (Identity String))
  quickCheck (monoidRightIdentity :: RightIdentity (Identity String))
  quickCheck (semigroupAssociativity :: Associativity (Two String String))
  quickCheck (monoidLeftIdentity :: LeftIdentity (Two String String))
  quickCheck (monoidRightIdentity :: RightIdentity (Two String String))
  quickCheck (semigroupAssociativity :: Associativity (Three String String String))
  quickCheck (semigroupAssociativity :: Associativity (Four String String String String))
  quickCheck (semigroupAssociativity :: Associativity BoolConj)
  quickCheck (monoidLeftIdentity :: LeftIdentity BoolConj)
  quickCheck (monoidRightIdentity :: RightIdentity BoolConj)
  quickCheck (semigroupAssociativity :: Associativity BoolDisj)
  quickCheck (monoidLeftIdentity :: LeftIdentity BoolDisj)
  quickCheck (monoidRightIdentity :: RightIdentity BoolDisj)
  quickCheck (semigroupAssociativity :: Associativity (Or String String))
  quickCheck (combineAssociativity :: FunctionAssociativity (Combine Int String) Int)
  quickCheck (combineLeftIdentity :: FunctionIdentity (Combine Int String) Int)
  quickCheck (combineRightIdentity :: FunctionIdentity (Combine Int String) Int)
  quickCheck (compAssociativity :: FunctionAssociativity (Comp String) String)
  quickCheck (semigroupAssociativity :: Associativity (Validation String Int))
  quickCheck (compLeftIdentity :: FunctionIdentity (Comp String) String)
  quickCheck (compRightIdentity :: FunctionIdentity (Comp String) String)
  quickCheck (memAssociativity :: FunctionAssociativity (Mem Int String) Int)
  quickCheck (memLeftIdentity :: FunctionIdentity (Mem String String) String)
  quickCheck (memRightIdentity :: FunctionIdentity (Mem String String) String)
