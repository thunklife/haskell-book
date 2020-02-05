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

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
  where
    gameLength :: String -> Bool
    gameLength s = 
      let l = length s
      in l >= minWordLength && l < maxWordLength

randomWord' :: WordList -> IO String
randomWord' wl = (wl !!) <$> randomRIO (0, l)
    where l = length wl - 1

randomWord :: IO String
randomWord = gameWords >>= randomWord'

main :: IO ()
main = forever $ getLine >>= putStrLn
