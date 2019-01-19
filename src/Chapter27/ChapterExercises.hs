-- {-# LANGUAGE Strict #-}

module Chapter27.ChapterExercises where

data List a =
  Nil |
  Cons a (List a)
  deriving (Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) =
  (Cons x (take' (n-1) xs))

map' _ Nil = Nil
map' f (Cons x xs) =
  (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)

-- main = do
--   print $ take' 10 $ map' (+1) (repeat' 1)



-- 1. let x = 1
-- x = _

-- 2. let x = ['1']
-- x = "1"

-- 3. let x = [1]
-- x = _

-- 4. let x = 1 :: Int
-- x = 1

-- 5. let f = \x -> x
--    let x = f 1
-- x = _ 

-- 6. let f :: Int -> Int; f = \x -> x
--    let x = f 1
-- x = _ 


-- 1. snd (undefined, 1)
-- 1

-- 2. let x = undefined
--    let y = x `seq` 1 in snd (x, y)
-- Exception

-- 3. length $ [1..5] ++ undefined
-- Exception

-- 4. length $ [1..5] ++ [undefined]
-- 6

-- 5. const 1 undefined
-- 1

-- 6. const 1 (undefined `seq` 1)
-- 1

-- 7. const undefined 1
-- Exception

x = undefined
y = "blah"
main = do
  print (snd (x , x `seq` y))
