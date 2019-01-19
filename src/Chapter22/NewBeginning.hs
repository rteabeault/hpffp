module Chapter22.NewBeginning where

import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- The previous function composition could be written as such
bloop :: Integer -> Integer
bloop = fmap boop doop

-- Using fmap here lifts the one partially applied function over the next,
-- in a sense setting up something like this:
-- fmap boop doop x == (*2) ((+10) x)
-- when this x comes along, it's the
-- first necessary argument to (+10)
-- then the result for that is the
-- first necessary argument to (*2)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

