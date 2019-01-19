module Chapter26.Test where
  
import Control.Applicative

data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }


instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return = pure
  x >>= f = EitherIO $ runEitherIO x >>= (\eea -> either (return . Left) (runEitherIO . f) eea)

-- instance Monad (EitherIO e) where
--   return = pure
--   (EitherIO eio) >>= f = EitherIO $ do
--     eeb <- eio
--     case eeb of
--       Left e -> return $ Left e
--       Right a -> runEitherIO $ f a

bad :: EitherIO String Int
bad = EitherIO $ do
  putStrLn "bad"
  return $ Left "bummer"

good :: EitherIO String Int
good = EitherIO $ do
  putStrLn "good"
  return $ Right 100

lift :: IO a -> EitherIO e a
lift x = EitherIO (fmap Right x)

ex1 = do
  v1 <- good
  v2 <- bad
  v3 <- good
  return (v1, v2) 
