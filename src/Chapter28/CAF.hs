module Chapter28.CAF where

incdInts :: [Integer]
incdInts = map (+1) [1..]

main :: IO ()
main = do
  print (incdInts !! 1000)
  print (incdInts !! 9001)
  print (incdInts !! 90010)
  print (incdInts !! 9001000)
  print (incdInts !! 9501000)
  print (incdInts !! 9901000)


-- not a CAF
-- incdInts :: [Integer] -> [Integer]
-- incdInts x = map (+1) x

-- main :: IO ()
-- main = do
--   print (incdInts [1..] !! 1000)
