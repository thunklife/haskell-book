module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

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

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
   when (length guessed > length wordToGuess) $
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  when (all isJust filledInSoFar) $
    do putStrLn "You win!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord
  let puzzle = freshPuzzle $ fmap toLower word
  runGame puzzle

