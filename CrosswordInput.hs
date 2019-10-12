module CrosswordInput where

import System.IO
import Data.Char
import Data.List
import Text.Read
import Data.Maybe

--OUR MODULES
import CrosswordConstants

-- the InputState is the list of words the user input,
-- the size of board they want,
-- and the number of builds they want to attempts
data InputState = InputState [[Char]] Int Int

-- INPUT VALIDATION CHECKS
nNotNum Nothing = True
nNotNum _ = False 
invalidBoardSize n = (n < minBoardSize) || (n > maxBoardSize)
invalidAttemptNumber n = (n <= 0) || (n > maxAttemptNumber)
validLength n word = (len <= n && len >= minWordLength) where len = length word
validChars [] = True
validChars (h:t) = (h `elem` validCharSet) && validChars t

-- HELPERS

-- Ordering function for sorting lists by length
compareLength :: String -> String -> Ordering
compareLength a b
    | l1 >= l2 = LT
    |otherwise = GT
    where l1 = length a
          l2 = length b



-- MAIN FUNCTIONS

-- get board size and word list from the user
getUserInput :: IO InputState
getUserInput =
    do
        putStrLn ("What size should your puzzle be? Enter number of rows:")
        boardSize <- getLine

        let n = (readMaybe boardSize) :: Maybe Int

        if nNotNum n
            then
                do
                    putStrLn ("Please enter a valid integer")
                    getUserInput
            else
                if invalidBoardSize (fromMaybe (-1) n)
                    then
                        do
                            putStrLn ("Invalid size, please choose a board size from "++ show minBoardSize ++" to " ++ show maxBoardSize ++".")
                            getUserInput
                    else
                        getDesiredAttempts (fromMaybe (-1) n)

-- Accepts a board size and gets the number of attempts the user wants to try
getDesiredAttempts :: Int -> IO InputState
getDesiredAttempts boardSize =
    do
        putStrLn ("How many attempts should we make to get an optimal puzzle?")
        input <- getLine

        let n = (readMaybe input) :: Maybe Int

        if nNotNum n
            then
                do
                    putStrLn ("Please enter a valid integer")
                    getDesiredAttempts boardSize
            else
                if invalidAttemptNumber (fromMaybe (-1) n)
                    then
                        do
                            putStrLn ("Please choose a number of attempts between 0 and " ++ show maxAttemptNumber ++".")
                            getDesiredAttempts boardSize
                    else
                        getWords boardSize (fromMaybe (-1) n) []               


getWords :: Int -> Int -> [[Char]] -> IO InputState
getWords boardSize attempts words =
    do
        putStrLn ("Enter a word between 2 and " ++ show boardSize ++" letters long. Enter " ++ doneLoading ++ " to build your puzzle!")
        word <- getLine
        if (word == doneLoading)
            then
                return (InputState words boardSize attempts)

        else if not (validLength boardSize word)
            then
                do
                    putStrLn("\"" ++word++ "\" is invalid length (words must be between " ++ show minWordLength ++
                             " and " ++ show boardSize ++ " letters on your chosen board size")
                    getWords boardSize attempts words
        else if not (validChars word)
            then
                do
                    putStrLn("\"" ++word++ "\" is not a valid word (only letters a-z are allowed)")
                    getWords boardSize attempts words
        
        else getWords boardSize attempts (sortBy compareLength ((map toUpper word):words))