module Ch5 where

mappity :: (a -> b) -> [a] -> [b]
mappity f = foldr ((:) . f) []



f1 :: a -> a -> a
f1 a _ = a

g1 :: a -> a -> a
g1 _  b = b

trpl :: Num a => a -> a
trpl = (*3)

myId :: a -> a


myId x = x
