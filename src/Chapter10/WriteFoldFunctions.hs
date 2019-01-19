module Chapter10.WriteFoldFunctions where
  
import Data.Time 

data DatabaseItem = DbString String
                  deriving (Eq, Ord, Show)
                  
theDatabase :: [DatabaseItem] 
theDatabase =
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]
  
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = undefined

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = undefined

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined

sumDb :: [DatabaseItem] -> Integer
sumDb = undefined

-- You'll probably need to use fromIntegral
avgDb = undefined
