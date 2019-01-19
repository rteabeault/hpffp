{-# LANGUAGE RankNTypes #-}

module Chapter24.LearnParsers where 
  
import Text.Trifecta
import Text.Parser.Combinators

stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

-- 1
oneEOF = one >> eof
oneTwoEof = one >> eof

-- 2
oneS = string "1"
oneTwoS = string "12"
oneTwoThreeS = string "123"

oneTwoThree :: Parser String
oneTwoThree = choice [ oneTwoThreeS, oneTwoS, oneS] <* eof

-- 3
stringFromChar :: String -> Parser String
stringFromChar = traverse char

--

testParse' :: Parser String -> String -> Result String
testParse' p = parseString p mempty

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "Ex 2. parse '1'"
  print $ testParse' oneTwoThree "1"
  pNL "Ex 2. parse '12'"
  print $ testParse' oneTwoThree "12"
  pNL "Ex 2. parse '123'"
  print $ testParse' oneTwoThree "123"
  pNL "Ex 2. parse '1234' - Should fail"
  print $ testParse' oneTwoThree "1234"
