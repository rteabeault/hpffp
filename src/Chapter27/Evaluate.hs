module Chapter27.Evaluate where
    
-- const :: a -> b -> a

-- 1. const 1 undefined
-- (\a -> (\b -> a)) 1 undefined
-- (\b -> 1) undedined 
-- 1

-- 2. const undefined 1
-- (a -> (\b -> a)) undefined 1
-- (\b -> undefined) 1
-- Exception 

-- 3. flip const undefined 1
-- (\b -> (\c -> c)) undefined 1
-- (\c -> c) 1
-- 1

-- 4. flip const 1 undefined
-- (\b -> (\c -> c)) 1 undefined
-- (\c -> c) undefined
-- Exception

-- 5. const undefined undefined
-- (\a -> (\b -> a)) undefined undefined
-- (\b -> undefined) undefined
-- Exception

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- foldr k z xs = go xs where
-- go [] = z
-- go (y:ys) = y `k` go ys

-- 6. foldr const 'z' ['a'..'e']
-- go ['a'..'e'] where
--   go [] = 'z'
--   go ('a':['b'..'e']) = 'a' `const` go ['b'..'e']
--
-- 'a' `const` go ['b'..'e']
-- 'a'

-- 7. foldr (flip const) 'z' ['a'..'e']
-- go ['a'..'e'] where
--  go [] = 'z'
--  go ('a':['b'..'e']) = 'a' `(flip const)` go ['b'..'e']

-- flip const will return go ['b'.'e'] and recursively go through each element until reaching []
-- once it reaches [] it will return 'z'
-- 'z' 
