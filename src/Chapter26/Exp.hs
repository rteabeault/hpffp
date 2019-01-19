{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter26.Exp where
  
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad

data AppError = DbSqlException String | MissingFoo

data AppEnv = AppEnv { appEnvConnection :: String }

newtype App a = App { unApp :: (ExceptT AppError (ReaderT AppEnv IO) a) }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader AppEnv
    , MonadError AppError
    , MonadIO)

runApp :: AppEnv -> App a -> IO (Either AppError a)
-- runApp e = runExceptT . flip runReaderT e . unApp
runApp = undefined
