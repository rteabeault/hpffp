module Chapter15.MaybeAnotherMonoid where

import Chapter15.MonoidLaws
import Chapter15.OptionalMonoid
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada 
  First' (Only a) `mappend` _ = First' (Only a)
  First' Nada `mappend` First' (Only a) = First' (Only a)
  First' Nada `mappend` First' Nada = First' Nada

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return $ First' Nada), (3, return (First' $ Only x))]

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
