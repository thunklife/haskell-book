module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = lines <$> readFile "data/dict.txt"

minWordLength :: Integer
minWordLength = 5

maxWordLength :: Integer
maxWordLength = 9

main :: IO ()
main = forever $ getLine >>= putStrLn
