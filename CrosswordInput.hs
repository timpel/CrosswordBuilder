module CrosswordInput where

import System.IO
import Data.Char
import Data.List
import Text.Read
import Data.Maybe

--OUR MODULES
import CrosswordConstants

-- the InputState is the list of words the user input and the size of board they want
data InputState = InputState [[Char]] Int

-- INPUT VALIDATION CHECKS
nNotNum Nothing = True
nNotNum _ = False 
invalidBoardSize n = (n < minBoardSize) || (n > maxBoardSize)
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

        --TODO: Check that this is a number ourselves - right now non numbers just crash
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
                        getWords (fromMaybe (-1) n) []
                        


getWords :: Int -> [[Char]] -> IO InputState
getWords n words =
    do
        putStrLn ("Enter a word between 2 and " ++ show n ++" letters long. Enter " ++ doneLoading ++ " to build your puzzle!")
        word <- getLine
        if (word == doneLoading)
            then
                return (InputState words n)

        else if not (validLength n word)
            then
                do
                    putStrLn("\"" ++word++ "\" is invalid length (words must be between " ++ show minWordLength ++
                             " and " ++ show n ++ " letters on your chosen board size")
                    getWords n words
        else if not (validChars word)
            then
                do
                    putStrLn("\"" ++word++ "\" is not a valid word (only letters a-z are allowed)")
                    getWords n words
        
        else getWords n (sortBy compareLength ((map toUpper word):words))