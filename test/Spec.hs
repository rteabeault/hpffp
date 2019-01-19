import Data.Monoid
import Test.QuickCheck

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (monoidLeftIdentity :: Maybe String -> Bool)
  quickCheck (monoidRightIdentity :: Maybe String -> Bool)
  quickCheck (monoidAssoc :: Maybe String -> Maybe String -> Maybe String -> Bool)
