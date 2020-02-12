module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderGameChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word nothings []
  where nothings :: [Maybe Char] 
        nothings = map (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = (`elem` word)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = (`elem` guessed)

renderGameChar :: Maybe Char -> Char
renderGameChar Nothing = '_'
renderGameChar (Just char) = char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where
    zipper :: Char -> Char -> Maybe Char -> Maybe Char 
    zipper guessed wordChar fillChar = if wordChar == guessed then Just wordChar else fillChar

    newFilledInSoFar :: [Maybe Char]
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

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
