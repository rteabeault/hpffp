{-# LANGUAGE InstanceSigs #-}

module Chapter23.State where
  
newtype State' s a =
  State' { runState' :: s -> (a, s) }

-- runState' ((+1) <$> (State' $ \s -> (0, s))) 0
-- (1,0)
instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \s -> let (a, b) = g s
                                     in (f a, b)

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure a = State' $ \s -> (a, s)

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  (State' f) <*> (State' g) = State' $ \s -> let (fab, s') = f s
                                                 (a, s'') = g s'
                                             in (fab a, s'')

instance Monad (State' s) where
  return = pure
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = State' $ \s -> let (a, s') = f s
                                        sb = runState' $ g a
                                        in sb s'

-- 1. Construct a State where the state is also the value you return.
get :: State' s s
get = State' $ \s -> (s, s)

-- 2. Construct a State where the resulting state is the argument provided and the value is defaulted to unit.
put :: s -> State' s ()
put s = State' $ \s -> ((), s)

-- 3. Run the State with 𝑠 and get the state that results.
exec :: State' s a -> s -> s
exec (State' sa) s = let (_, s') = sa s
                     in s'

-- 4. Run the State with 𝑠 and get the value that results.
eval :: State' s a -> s -> a
eval (State' sa) s = let (a, _) = sa s
                     in a

-- 5. Write a function which applies a function to create a new State.
modify :: (s -> s) -> State' s ()
modify f = State' $ \s -> ((), f s) 

