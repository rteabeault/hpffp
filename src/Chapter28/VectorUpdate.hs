module Chapter28.VectorUpdate where

import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import Data.Vector ((//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

vec :: V.Vector Int
vec = V.fromList [1..10000]

-- one update at a time using the batch update operator //
slow :: Int -> V.Vector Int
slow n = go n vec
  where go 0 v = v
        go n v = go (n - 1) (v // [(n, 0)])

-- collect all updates and then apply them all at once
batchList :: Int -> V.Vector Int
batchList n = vec // updates
  where updates =
          fmap (\n -> (n, 0)) [0..n]

batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
  where updates =
          fmap (\n -> (n, 0)) (V.fromList [0..n])

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n + 1)
  go n mvec
    where go 0 v = return v
          go n v = (MV.write v n 0) >> go (n - 1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n + 1)
  go n mvec
    where go 0 v = V.freeze v
          go n v = (MV.write v n 0) >> go (n - 1) v

main :: IO ()
main = defaultMain
  [ bench "slow" $ whnf slow 9998
  , bench "batch list" $ whnf batchList 9998
  , bench "batch vector" $ whnf batchVector 9998
  , bench "mutable IO vector" $ whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector" $ whnf mutableUpdateST 9998]
