module Chapter28.ProfilingMemory where

import Control.Monad

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main =
  replicateM_ 10000 (print blah)
