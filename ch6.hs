module Chapter6 where

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

instance Eq DayOfWeek where
  Mon == Mon = True
  Tue == Tue = True
  Wed == Wed = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  Sun == Sun = True
  _ == _ = False


data Five = One | Two | Three | Four | Five

instance Show Five where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"

newtype TisAnInteger =
  TisA Integer

instance Eq TisAnInteger where
  (TisA a) == (TisA b) = a == b

data TwoIntegers =
  TwoIntegers Integer Integer

instance Eq TwoIntegers where
  (TwoIntegers a b) == (TwoIntegers a' b') = a == a' && b == b'

data IntOrString =
  TisAnInt Int
  | TisAString String

instance Eq IntOrString where
  (TisAnInt a) == (TisAnInt a') = a == a'
  (TisAString a) == (TisAString a') = a == a'
  _ == _ = False
