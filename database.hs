module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime

type Database = [DatabaseItem]

theDatabase :: Database 
theDatabase =
  [ DbDate (UTCTime
      (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
  ]

filterDbDate :: Database -> [UTCTime]
filterDbDate = foldr f []
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate date) acc = date:acc
    f _ acc = acc

filterDbNumber :: Database -> [Integer]
filterDbNumber = foldr f []
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber num) acc = num:acc
    f _ acc = acc
