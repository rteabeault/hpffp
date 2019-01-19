module Chapter9.Cipher where
  
import Data.Char

caesar :: String -> String
caesar = map (rotateChar 3)

uncaesar :: String -> String
uncaesar = map (rotateChar (-3))

rotateChar :: Int -> Char -> Char
rotateChar rot c
  | isAsciiLower c = chr (rotate ascii rot 97 122)
  | isAsciiUpper c = chr (rotate ascii rot 65 90)
  | otherwise = c
  where ascii = ord c


rotate :: Int -> Int -> Int -> Int -> Int
rotate val rot lowerBound upperBound
  | rotated > upperBound = rotated - length 
  | rotated < lowerBound = rotated + length 
  | otherwise = rotated
  where rotated = val + rot
        length = upperBound - lowerBound + 1