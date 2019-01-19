module Chapter18.FmapJoin where

import Control.Monad (join)

-- Implement bind using fmap and join
-- fmap :: (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
