module Chapter28.MapVSet where

import Criterion.Main
import qualified Data.Map.Strict as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s



main :: IO ()
main = defaultMain
  [ bench "intersection map" $ whnf (M.intersection m) m
  , bench "intersection set" $ whnf (S.intersection s) s
  , bench "union map" $ whnf (M.union m) m
  , bench "union set" $ whnf (S.union s) s]
