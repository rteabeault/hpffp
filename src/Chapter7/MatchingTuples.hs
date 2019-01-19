module Chapter7.MatchingTuples where
  
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y
 
addEmUp2' :: Num a => (a, a) -> a
addEmUp2' tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third3 :: (a, b, c) -> c
third3 (_, _, c) = c

k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4-1), 10)

k2 :: String 
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)


f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))
