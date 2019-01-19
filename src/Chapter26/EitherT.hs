{-# LANGUAGE InstanceSigs #-}

module Chapter26.EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }


instance Functor m
      => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema



instance Applicative m
      => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure x = EitherT $ (pure . pure) x

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emf) <*> (EitherT ema) = EitherT $ (<*>) <$> emf <*> ema


instance Monad m
      => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f = EitherT $ do
    v <- ema
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT (f a)


swapEitherT :: (Functor m) =>
     EitherT e m a
  -> EitherT a m e
swapEitherT (EitherT ema) =
  let f = (\e -> case e of
              Left e -> Right e
              Right a -> Left a)
  in EitherT $ fmap f ema


eitherT :: Monad m =>
     (a -> m c)
  -> (b -> m c)
  -> EitherT a m b
  -> m c
eitherT fa fb (EitherT amb) = amb >>= either fa fb
