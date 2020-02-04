module Chapter10 where

mySum :: [Integer] -> Integer
mySum = foldr (+) 0

myLength :: [a] -> Integer
myLength = foldr (\ _ acc -> acc + 1) 0

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []
