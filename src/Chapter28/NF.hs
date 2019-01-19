module Chapter28.NF where
  
import Criterion.Main

myList :: [Int]
myList = [1..9999]
-- myList = (undefined : [2..9999])
-- myList = (undefined : undefined)
-- myList = undefined

main :: IO ()
main = defaultMain
  [ bench "map list 9999" $ nf (map (+1)) myList ]
