module Chapter16.HeavyLifting where

-- 16.7 Exercises

-- fmap :: Functor f => (a -> b) -> f a -> f b

-- 1.
-- a = (+1) $ read "[1]" :: [Int]
-- Expexted Result: [2]

a = fmap (+1) $ read "[1]" :: [Int]

-- 2.
-- b = (++ "lol") (Just ["Hi,", "Hello"])
-- Expected Result: Just ["Hi,lol","Hellolol"]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
-- c = (*2) (\x -> x - 2)
-- Expected Result: c 1
-- -2

c = fmap (*2) (\x -> x - 2)

-- 4.
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- Expected Result: d 0
-- "1[0,1,2,3]"

-- :t ((return '1' ++))
-- ((return '1' ++)) :: [Char] -> [Char]

-- :t show
-- show :: Show a => a -> String

-- :t ((return '1' ++) . show)
-- ((return '1' ++) . show) :: Show a => a -> [Char]

-- :t fmap ((return '1' ++) . show) 
-- fmap ((return '1' ++) . show) :: (Show a, Functor f) => f a -> f [Char]

-- :t fmap ((return '1' ++) . show) (\x -> [x, 1..3])
-- fmap ((return '1' ++) . show) (\x -> [x, 1..3]) :: (Enum a, Num a, Show a) => a -> [Char]

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5.
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--        changed = read ("123"++) show ioi
--    in (*3) changed
-- Expected Result: 3693

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
