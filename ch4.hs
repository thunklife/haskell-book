module Ch4 where

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]

also :: [String]
also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

isPalindrome :: String -> Bool
isPalindrome str = reverse str == str

myAbs :: Integer -> Integer
myAbs n
  | n >= 0 = n
  | otherwise = negate n

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))
