module Chapter17.Validation where
  
import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Errors =
   DividedByZero
 | StackOverflow
 | MooglesChewedWires deriving (Eq, Show)

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success a = Success (f a)
  Success f <*> Failure e = Failure e
  Failure e <*> Success a = Failure e
  Failure e <*> Failure e' = Failure (e <> e')

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  putStrLn "Validation applicative test"
  quickBatch (applicative (undefined :: Validation String (Int, String, Bool)))
