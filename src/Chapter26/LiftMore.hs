{-# LANGUAGE InstanceSigs #-}

module Chapter26.LiftMore where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Chapter26.EitherT
import Chapter26.MaybeT
import Chapter26.StateT
import Chapter26.ReaderT
import Control.Monad (liftM)

instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m a -> EitherT e m a
  lift = EitherT . liftM Right 

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m)
       => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m)
       => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m)
       => MonadIO (StateT s m) where
  liftIO = lift . liftIO
