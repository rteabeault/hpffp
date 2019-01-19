module Chapter16.IgnoringPosibilities where

-- 16.11

-- Maybe
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Either
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too!")
