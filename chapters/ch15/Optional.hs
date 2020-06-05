module Optional where

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only (x <> y)
  (<>) (Only x) Nada = Only x
  (<>) Nada (Only x) = Only x
  (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
