module Ch8 where

multiply :: Integer -> Integer -> Integer 
multiply 0 _ = 0
multiply _ 0 = 0
multiply x y = x + multiply x (y - 1)
