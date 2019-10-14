module Crossword where

import System.IO
import System.Random
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

-- OUR MODULES
import CrosswordInput
import CrosswordConstants

-- DATA TYPES

{- The State of the Crossword. Contains (in order):
    - A map of boards coordinates to the letter placed at that coordinate.
    - A list of unplaced words.
    - An infinite list of random doubles.
    - The size of the board (an n x n board).
-}
data State = State 
                (Map.Map (Int, Int) Char) -- Map of (x,y) coordinates -> the letter in that cell
                [[Char]]                  -- List of words that we haven't been able to place yet
                [Double]                  -- Infinite list of random doubles 
                Int                       -- size of the board

-- The direction of a word on the board. Can be horizontal or vertical.
data Direction = Horizontal
                | Vertical
    deriving Eq

{- An intersection represents a potential placement of a letter in a word on the board. Contains:
    - The letter.
    - The index of the letter in the word being placed.
    - A pair of Ints representing x-y board coordinates for the placement location.
-}
data Intersection = Intersection Char Int (Int, Int)
    deriving Show

-- ENTRY POINTS

{-
Calling start will prompt the user for necessary board inputs (board size,
number of build attempts, and a list of words), then build the crossword.
-}
start :: IO ()
start = buildFromIO getUserInput

{-
An alternative entry point, called using the format: 'startFromFile "inputfilename" boardSize attempts', where:
  - "inputfilename" is the name of a file containing a list of words separated by tabs, commas, and/or newlines.
  - boardSize is the board size.
  - attempts is the number of build attempts to make.
-}
startFromFile :: [Char] -> Int -> Int -> IO ()
startFromFile fileName boardSize timesToTry =
    do
        -- validate boardSize and number of attempts
        if (invalidBoardSize boardSize) || (invalidAttemptNumber timesToTry)
        then do
            putStrLn("Usage: startFromFile filename boardSize buildAttempts")
            putStrLn ("where boardSize is in the range [2, " ++ show maxBoardSize ++ "] and buildAttempts is in the range [1, " ++ show maxAttemptNumber ++ "].")
        else do
            file <- readFile fileName
            let splitFile = splitOneOf "\n,;:.\t" (filter (/= ' ') file)
            let rawInputList = map (map toUpper) (filter (/= "") splitFile)
            let inputList = sortBy compareLength (filter (validWord boardSize) rawInputList)
            let invalidWords = rawInputList \\ inputList

            printInvalid invalidWords
            
            putStrLn ("Building a puzzle out of " ++ show inputList)

            randomDubs <- makeRandomGen
            let initState = State Map.empty [] randomDubs boardSize
            let prevState = State Map.empty inputList randomDubs boardSize
            let (State letterMap unplacedWords rndDoubles boardSize) = build initState inputList [] prevState inputList timesToTry

            printBoard (makeBoard letterMap boardSize 0) boardSize
            putStrLn("Failed to place " ++ show (length unplacedWords) ++ " words: " ++ show unplacedWords)

-- BUILDING THE CROSSWORD

{- Build a crossword from the given InputState, which consists of:
    - a list of words to place
    - a board size
    - a number of build attempts to try
-}
buildFromIO :: IO InputState -> IO ()
buildFromIO input =
    do
        (InputState inputList boardSize timesToTry) <- input
        putStrLn ("Building a puzzle out of " ++ show inputList)

        randomDubs <- makeRandomGen
        let initState = State Map.empty [] randomDubs boardSize
        let prevState = State Map.empty inputList randomDubs boardSize
        let (State letterMap unplacedWords rndDoubles boardSize) =
                    build initState inputList [] prevState inputList timesToTry

        printBoard (makeBoard letterMap boardSize 0) boardSize
        putStrLn("Failed to place " ++ show (length unplacedWords) ++ " words: " ++ show unplacedWords)


{- Build is the main function used in building a crossword.
    - It takes the following parameters (in order):
        - The current state of the board
        - A list of words left to place for this build
        - A list of words that couldn't be placed in previous attempts of this build
        - The best state seen so far (across all previous builds)
        - The full list of words input that we would like to place
        - The numbers of builds left to try
    - It is called recursively in several different ways:
        - If there are still words left to place, an attempt is made to place the word.
            - If the word is placed, then placedLetters is updated.
            - If the word is not placed, then the list of unplaced words is updated.
            - Either way, build is recursively called with the remaining list of words to place.
        - When the list of words left to place for this build is empty:
            - If all input words have been successfully placed, then success!
                - A full crossword puzzle has been generated, and the state generated by this build attempt is returned.
            - Else if no new words were placed on this 'iteration' of the build, then this build has reached a deadend.
                - If there are remaining build attempts to try, move on to the next attempt.
                - If there are no remaining build attempts to try, return the best state that has been generated in any build attempt.
            - Else this 'iteration' of the build has made progress, and new intersections are available to potentially place words.
                - Continue this build attempt: recursively call build and attempt to place all previously unplaceable words.
    - Build returns either the bestStateSoFar or the state produced in this build attempt -- whichever has fewer unplaced words.
-}
build :: State -> [[Char]] -> [[Char]] -> State -> [[Char]] -> Int -> State
build state [] prevUnplaced bestStateSoFar initialList toTry
    | unplaced == [] = state
    | (sort unplaced) == (sort prevUnplaced) =
        if (toTry == 0)
            then
                newBestState
            else
                build (State Map.empty [] rnd boardSize) initialList [] newBestState initialList (toTry - 1)
    | otherwise = build (State letterMap [] rnd boardSize) unplaced unplaced bestStateSoFar initialList toTry
    where
        newBestState = bestOf state bestStateSoFar
        (State letterMap unplaced rnd boardSize) = state

build state (h:t) prevUnplaced bestStateSoFar initialList toTry
    | null letterMap = build (placeFirst state h) t prevUnplaced bestStateSoFar initialList toTry
    | otherwise = build (tryToPlace state h intersections) t prevUnplaced bestStateSoFar initialList toTry
    where
        (State letterMap unplaced rnd boardSize) = state
        intersections = randomizedList (getAllIntersections h letterMap 0) rnd

-- PLACING WORDS

{- Place the first word, horizontally, in any random place where it'll fit.
    - Parameters:
        - the current state,
        - the word to place.
    - Returns: the updated state.
-}
placeFirst :: State -> [Char] -> State
placeFirst (State letterMap unplacedWords rnd boardSize) word =
    (State newLetterMap [] newRandList boardSize)
    where
        (rand1:rand2:newRandList) = rnd
        randomValidIdx rndDouble str = randIntFromDouble 0 (boardSize - (length str) + 1) rndDouble
        newLetterMap = addToPlaced letterMap (randomValidIdx rand1 "", randomValidIdx rand2 word) word Horizontal


{- Attempt to place a word on the board.
    - Parameters:
        - the current state,
        - the word to place,
        - a list of intersections for the word to place.
    - Returns: the updated state (with either placedLetters or unplacedWords updated).
-}
tryToPlace :: State -> [Char] -> [Intersection] -> State
-- if there are no intersections to work with, add the word to the Unplaced Words list
tryToPlace (State letterMap unplaced rnd boardSize) word [] =
    (State letterMap (word : unplaced) rnd boardSize)

-- Go through the intersections unti we find one we can place this word at, either horizontally or vertically
tryToPlace (State letterMap unplacedWords rnd boardSize) word ((Intersection letter idx (row, col)) : t)
        | canPlace Horizontal = State (placeIt Horizontal) unplacedWords rnd boardSize
        | canPlace Vertical = State (placeIt Vertical) unplacedWords rnd boardSize
        | otherwise = tryToPlace (State letterMap unplacedWords rnd boardSize) word t
    where
        canPlace direction = checkPlacement letterMap word (startCell direction) direction boardSize
        startCell direction = getStartCell idx (row, col) direction
        placeIt direction = addToPlaced letterMap (startCell direction) word direction

{- Checks whether placing the given word at the given starting point will conflict with any existing words.
    - Parameters:
        - the letters placed on the board so far,
        - the word to place,
        - x-y coordinates for the starting point,
        - the direction in which the word will be placed,
        - the board size.
    - Returns: True if the word can be placed.
-}
checkPlacement :: (Map.Map (Int, Int) Char) -> [Char] -> (Int, Int) -> Direction -> Int -> Bool
checkPlacement letterMap word startCell direction boardSize =
    doesTheWordFit letterMap word startCell direction boardSize &&
    canEachLetterBePlaced letterMap word startCell direction boardSize True

{- Add all the letters in the given word to the board.
    - Warning: This makes no checks, so make sure that this is a good placement before calling it!
    - Parameters:
        - the letters placed on the board so far,
        - x-y coordinates for the starting point of the word being placed,
        - the word being placed,
        - the direction in which the word will be placed,
    - Returns: The updated letters placed on the board so far.
-}
addToPlaced :: Map.Map (Int, Int) Char -> (Int, Int) -> [Char] -> Direction -> Map.Map (Int, Int) Char
addToPlaced letterMap cell [] direction = letterMap
addToPlaced letterMap cell (letter:restOfWord) direction =
    Map.insert cell letter (addToPlaced letterMap 
                                    (nextCell cell direction)
                                    restOfWord
                                    direction)


-- GETTING ALL POSSIBLE INTERSECTIONS

-- Get all intersections between this word and any currently placed words
getAllIntersections :: [Char] -> (Map.Map (Int, Int) Char) -> Int -> [Intersection]
getAllIntersections [] letterMap idx = []
getAllIntersections (h:t) letterMap idx =
    (getLetterIntersections h idx letterMap) ++ getAllIntersections t letterMap (idx+1)

-- Get the coordinates of every cell that currently contains the given letter
getLetterIntersections :: Char -> Int -> (Map.Map (Int, Int) Char) -> [Intersection]
getLetterIntersections letter idx letterMap =
    map (Intersection letter idx) (getAllCellsWithLetter letterMap letter)

getAllCellsWithLetter :: Eq a1 => Map.Map a2 a1 -> a1 -> [a2]
getAllCellsWithLetter letterMap letter = matchingCells
        where
            (matchingCells, map) = Map.mapAccumWithKey (matchLetter) [] letterMap
            matchLetter listSoFar cell charToCheck =
                if charToCheck == letter
                    then (cell : listSoFar, letterMap)
                    else (listSoFar, letterMap)


-- PLACEMENT-CHECKING HELPERS

doesTheWordFit :: Foldable t => (Map.Map (Int, Int) Char) -> t a -> (Int, Int) -> Direction -> Int -> Bool
doesTheWordFit letterMap word startCell direction boardSize =
{-- The word fits on the board only if:
    1) There is no letter on the board in front of the first letter
    2) There is no letter on the board after the last letter
    3) Both the first and last letter are in the bounds of the board--}
    (inBounds startCell boardSize && inBounds lastCell boardSize &&
    freeOrOOB (prevCell startCell direction) letterMap boardSize &&
    freeOrOOB (nextCell lastCell direction) letterMap boardSize) where
        lastCell = getLastCell startCell direction word

canEachLetterBePlaced :: (Map.Map (Int, Int) Char) -> [Char] -> (Int, Int) -> Direction -> Int -> Bool -> Bool
canEachLetterBePlaced letterMap "" cell direction boardSize _ = True
canEachLetterBePlaced letterMap (thisLetter:nextLetters) cell direction boardSize wasLastCellEmpty =
    {-- A letter can be placed in a cell only if:
        1) The cell in the bounds of the board
        2) The cell is either
            i) empty AND the "flanking cells" (aka above & below for a horizontal word, left & right for a vertical one) are empty
            ii) OR the cell is not empty but its letter is the same as the given letter --}
    (inBounds cell boardSize) &&
    ((isEmpty cell letterMap && all (\ cell -> freeOrOOB cell letterMap boardSize) (flankingCells cell direction))
        || ((getLetterFromCell cell letterMap) == thisLetter) && wasLastCellEmpty) &&
    canEachLetterBePlaced letterMap nextLetters (nextCell cell direction) direction boardSize (isEmpty cell letterMap)

-- Gets the cells above & below this one for Horizontal words, or to the left & right for Vertical words
flankingCells :: (Num a1, Num a2) => (a1, a2) -> Direction -> [(a1, a2)]
flankingCells (row, col) Horizontal = [(row-1, col),(row+1, col)]
flankingCells (row, col) Vertical = [(row, col-1),(row, col+1)]



-- BOARD GENERATION & OUTPUT

-- Makes formatted string of size n*n filled with either placed letters or 'empty cell' characters
makeBoard :: (Map.Map (Int, Int) Char) -> Int -> Int -> [Char]
makeBoard letterMap n i
    | i >= totalCells = []
    | otherwise = space : letter : makeBoard letterMap n (i+1)
    where
        totalCells = n*n
        (space, letter) = (' ', (getLetterFromCell (i `div` n, i `mod` n) letterMap))

-- Prints out the board, one row of size n at a time
printBoard :: [Char] -> Int -> IO ()
printBoard "" n = putStrLn("")
printBoard boardString n =
    do
        putStrLn (take (2*n) boardString)
        printBoard (drop (2*n) boardString) n

-- HELPER/UTIL FUNCTIONS

-- bestOf returns the better of two states.
-- One state is considered better than another if the first placed more words than the second.
bestOf :: State -> State -> State
bestOf newState oldState =
    if ((length unplacedWordsNew) < (length unplacedWordsOld))
        then newState
        else oldState
    where
        (State _ unplacedWordsNew _ _) = newState
        (State _ unplacedWordsOld _ _) = oldState
    

-- get an infinite (lazy) list of random doubles (adapted from Prof. Poole's example)
makeRandomGen :: IO [Double]
makeRandomGen =
    do
        rg <- newStdGen
        return (randoms rg :: [Double])

-- Range includes loInt but excludes hiInt
randIntFromDouble :: Int -> Int -> Double -> Int
randIntFromDouble loInt hiInt rndDouble =
    floor ((hi-lo) * rndDouble + lo)
    where
        hi = fromIntegral hiInt
        lo = fromIntegral loInt

-- Needs infinite list of doubles
randomizedList :: [a] -> [Double] -> [a]
randomizedList [] _ = []
randomizedList (h:t) (d:rd) = randomInsert h (randomizedList t rd) d

randomInsert :: a -> [a] -> Double -> [a]
randomInsert e [] d = [e]
randomInsert e l d = before ++ [e] ++ after
  where (before, after) = splitAt (randIntFromDouble 0 ((length l) + 1) d) l


-- check if a given word is of valid length and has all valid characters
validWord :: Int -> [Char] -> Bool
validWord boardSize word = (validLength boardSize word) && (validChars word)

-- print all the words in the list, with messages for the user
printInvalid :: (Foldable t, Show (t a)) => t a -> IO ()
printInvalid invalidWords =
    if (not (null invalidWords))
        then
            do
                putStrLn ("The following words are invalid and won't be placed: " ++ show invalidWords)
        else
            do
                putStrLn ("Input file is valid - well done!")


-- Given a point, its index in a word, the word itself, and the direction of the word,
-- returns the point where the first letter of that word should be
getStartCell :: Num a => a -> (a, a) -> Direction -> (a, a)
getStartCell wordIndex (row, col) Horizontal = (row, col-wordIndex)
getStartCell wordIndex (row, col) Vertical = (row - wordIndex, col)

-- Get the cell where the last letter of this word will be placed given its starting cell
getLastCell :: Foldable t => (Int, Int) -> Direction -> t a -> (Int, Int)
getLastCell (row, col) Horizontal word = (row, col + (length word) - 1)
getLastCell (row, col) Vertical word = (row + (length word) - 1, col)

-- Get the letter placed at the given point (or the empty character if nothing is placed there)
getLetterFromCell :: (Int, Int) -> (Map.Map (Int, Int) Char) -> Char
getLetterFromCell cell letterMap = Map.findWithDefault emptyCellCharacter cell letterMap

-- Check if a cell is within the board
inBounds :: (Ord a, Num a) => (a, a) -> a -> Bool
inBounds (row, col) boardSize = (row>=0) && (row<boardSize) && (col>=0) && (col<boardSize)

-- Returns the cell to the left of this one if Direction=Horizontal,
-- Or the cell above it if Direction=Vertical
prevCell :: (Num b, Num a) => (a, b) -> Direction -> (a, b)
prevCell (row, col) Horizontal = (row, col-1)
prevCell (row, col) Vertical = (row-1, col)

-- Returns the cell to the right of this one if Direction=Horizontal,
-- Or the cell below it if Direction=Vertical
nextCell :: (Num b, Num a) => (a, b) -> Direction -> (a, b)
nextCell (row, col) Horizontal = (row, col+1)
nextCell (row, col) Vertical = (row+1, col)

-- Checks if the given cell is either outside the board area OR is currently empty
freeOrOOB :: (Int, Int) -> (Map.Map (Int, Int) Char) -> Int -> Bool
freeOrOOB cell letterMap boardSize = (not (inBounds cell boardSize)) || (isEmpty cell letterMap)

-- Checks if no letter has been placed has been placed in this cell yet
isEmpty :: (Int, Int) -> (Map.Map (Int, Int) Char) -> Bool
isEmpty cell letterMap = not (Map.member cell letterMap)