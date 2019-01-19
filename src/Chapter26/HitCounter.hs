{-# LANGUAGE OverloadedStrings #-}

module Chapter26.HitCounter where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           System.Environment (getArgs)
import           Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)


bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = do
  let updated = M.insertWith (\new old -> old + 1) k 1 m
  (updated, M.findWithDefault 1 k updated)

app :: Scotty ()
app =
  get "/:key" $ do
    config <- lift ask
    unprefixed <- param "key"
    let key' = mappend (prefix config) unprefixed
        ref' = counts config
        map' = readIORef ref'
    (updatedCounts, count) <- liftIO (bumpBoomp key' <$> map')
    liftIO $ writeIORef ref' updatedCounts
    html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show count, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
