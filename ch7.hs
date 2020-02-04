module Ch7 where

-- addFive x y = (if x > y then y else x) + 5
addFive :: (Ord a, Num a) => a -> a -> a
addFive =
  \x ->
    \y ->
      (if x > y
         then y
         else x) +
      5

-- mflip f = \x -> \y -> f y x
mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x

-- Users

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

instance Show User where
  show UnregisteredUser = "UnregisteredUser"
  show (RegisteredUser
        (Username a)
        (AccountNumber b)) = a ++ " " ++ show b
