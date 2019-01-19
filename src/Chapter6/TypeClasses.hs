module Chapter6.TypeClasses where
  
import Data.List (sort)

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False
  
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ
  
data Date = Date DayOfWeek Int deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') = 
    weekday == weekday' && dayOfMonth == dayOfMonth'
    
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y
    

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two w x) (Two y z) = w == y && x == z
  
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False
  
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'
  
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'
  
data Which a = 
    ThisOne a
  | ThatOne a
  
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a' 
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False
  
data EitherOr a b =
    Hello a
  | Goodbye b
  
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False
  
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Show, Eq, Ord)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f(a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = fromInteger i + f(a)  

