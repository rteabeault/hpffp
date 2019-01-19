module Chapter17.FixerUpper where
    
-- 1.
x = const <$> Just "Hello" <*> Just "World"

-- 2.
x' = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
