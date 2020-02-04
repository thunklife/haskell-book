module Cipher where

  import Data.Char (chr, ord)

  toOrds :: String -> [Int]
  toOrds = map ord

  toChrs :: [Int] -> String
  toChrs = map chr

  myId :: String -> String
  myId = toChrs . toOrds

  uppers :: String
  uppers = ['A'..'Z']

  lowers :: String
  lowers = ['a'..'z']
