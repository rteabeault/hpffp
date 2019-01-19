module Chapter15.OptionalMonoid where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Nada `mappend` o = o
  o `mappend` Nada = o
  Only o1 `mappend` Only o2 = Only(o1 `mappend` o2)
