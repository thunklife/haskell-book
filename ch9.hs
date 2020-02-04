module Ch9 where

  import Data.Char (toUpper)

  eftBool :: Bool -> Bool -> [Bool]
  eftBool a b
    | a > b = []
    | otherwise = [True]

  eftOrd :: Ordering -> Ordering -> [Ordering]
  eftOrd a b
    | a > b = []
    | a == b = [a]
    | otherwise = a : eftOrd (succ a) b

  eftInt :: Int -> Int -> [Int]
  eftInt a b
    | a > b = []
    | a == b = [a]
    | otherwise = a : eftInt (succ a) b

  isNothing :: Maybe a -> Bool
  isNothing Nothing = True
  isNothing _ = False

  myZip :: [a] -> [b] -> [(a,b)]
  myZip [] _ = []
  myZip _ [] = []
  myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

  myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  myZipWith _ [] _ = []
  myZipWith _ _ [] = []
  myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

  myZip2 :: [a] -> [b] -> [(a,b)]
  myZip2 = myZipWith (,)

  capitalize :: String -> String
  capitalize "" = ""
  capitalize (x:xs) = toUpper x : xs

  yell :: String -> String
  yell "" = ""
  yell (x:xs) = toUpper x : yell xs
