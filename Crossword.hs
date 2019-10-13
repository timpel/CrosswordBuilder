module Crossword where

import System.IO
import System.Random
import Data.Char
import Data.List
import Data.List.Split

-- OUR MODULES
import CrosswordInput
import CrosswordConstants

-- DATA TYPES

{- The State of the Crossword. Contains (in order):
    - A list of the current PlacedLetters on the board.
    - A list of unplaced words.
    - An infinite list of random doubles.
    - The size of the board (an n x n board).
-}
data State = State [PlacedLetter] [[Char]] [Double] Int

-- The direction of a word on the board. Can be horizontal or vertical.
data Direction = Horizontal
                | Vertical
    deriving Eq

{- Represents a letter placed on the board. Contains:
    - The letter.
    - A pair of Ints representing x-y board coordinates.
    - i.e. PlacedLetter 'A' (3, 6) represents the letter A in cell (3, 6).
-}
data PlacedLetter = PlacedLetter Char (Int, Int)
    deriving Show

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
An alternative entry point, called using the format: startFromFile "inputfilename" n m, where:
  - "inputfilename" is the name of a file containing a list of words separated by tabs, commas, and/or newlines.
  - n is the board size.
  - m is the number of build attempts to make.
-}
startFromFile :: [Char] -> Int -> Int -> IO ()
startFromFile fileName boardSize timesToTry =
    do
        file <- readFile fileName
        let splitFile = splitOneOf "\n,;:.\t" (filter (/= ' ') file)
        let rawInputList = map (map toUpper) (filter (/= "") splitFile)
        let inputList = filter (validWord boardSize) rawInputList
        let invalidWords = rawInputList \\ inputList

        printInvalid invalidWords

        putStrLn ("Building a puzzle out of " ++ show inputList)

        randomDubs <- makeRandomGen
        let initState = State [] [] randomDubs boardSize
        let prevState = State [] inputList randomDubs boardSize
        let (State placedLetters unplacedWords rndDoubles boardSize) = build initState inputList [] prevState inputList timesToTry

        printBoard (makeBoard placedLetters boardSize 0) boardSize
        putStrLn("Failed to place these words: " ++ show unplacedWords)


-- BUILDING THE CROSSWORD

-- Build a crossword from the given input words
buildFromIO :: IO InputState -> IO ()
buildFromIO input =
    do
        (InputState inputList boardSize timesToTry) <- input
        putStrLn ("Building a puzzle out of " ++ show inputList)

        randomDubs <- makeRandomGen
        let initState = State [] [] randomDubs boardSize
        let prevState = State [] inputList randomDubs boardSize
        let (State placedLetters unplacedWords rndDoubles boardSize) = build initState inputList [] prevState inputList timesToTry

        printBoard (makeBoard placedLetters boardSize 0) boardSize
        putStrLn("Failed to place these words: " ++ show unplacedWords)


{- Build takes (in order):
        - The current state of the board
        - A list of words left to place for this build
        - A list of words that couldn't be placed in previous attempts of this build
        - The best state seen so far (across all previous builds)
        - The full list of words input that we would like to place
        - The numbers of builds left to try

    It returns either the bestStateSoFar or the state produced in this build attempt -- whichever has fewer unplaced words.
-}
build :: State -> [[Char]] -> [[Char]] -> State -> [[Char]] -> Int -> State
build state [] prevUnplaced bestStateSoFar initialList toTry
    | unplaced == [] = state
    | (sort unplaced) == (sort prevUnplaced) =
        if (toTry == 0)
            then
                newBestState
            else
                build (State [] [] rnd n) initialList [] newBestState initialList (toTry - 1)
    | otherwise = build (State placedLetters [] rnd n) unplaced unplaced bestStateSoFar initialList toTry
    where
        newBestState = bestOf state bestStateSoFar
        (State placedLetters unplaced rnd n) = state
build state (h:t) prevUnplaced bestStateSoFar initialList toTry
    | null placedLetters = build (placeFirst state h) t prevUnplaced bestStateSoFar initialList toTry
    | otherwise = build (tryToPlace state h intersections) t prevUnplaced bestStateSoFar initialList toTry
    where
        (State placedLetters unplaced rnd n) = state
        intersections = randomizedList (getAllIntersections h placedLetters 0) rnd

bestOf state1 state2 =
    if ((length unplacedWords1) >= (length unplacedWords2))
        then state2
        else state1
    where
        (State _ unplacedWords1 _ _) = state1
        (State _ unplacedWords2 _ _) = state2


-- Place the first word, horizontally, in any random place where it'll fit
placeFirst :: State -> [Char] -> State
placeFirst (State placedLetters unplacedWords rnd n) word =
    (State newLetterPlacement [] newRandList n)
    where
        (rand1:rand2:newRandList) = rnd
        randomValidIdx rndDouble str = randIntFromDouble 0 (n - (length str) + 1) rndDouble
        newLetterPlacement = (addToPlaced [] (randomValidIdx rand1 "", randomValidIdx rand2 word) word Horizontal)


-- TESTING INTERSECTIONS & PLACING WORDS

tryToPlace :: State -> [Char] -> [Intersection] -> State
-- if there are no intersections to work with, add the word to the Unplaced Words list
tryToPlace (State placedLetters unplaced rnd n) word [] =
    (State placedLetters (word : unplaced) rnd n)

-- Go through the intersections until we find one we can place this word at, either horizontally or vertically
tryToPlace (State placedLetters unplacedWords rnd n) word ((Intersection letter idx (row, col)) : t)
        | canPlace Horizontal = (State (placeIt Horizontal) unplacedWords rnd n)
        | canPlace Vertical = (State (placeIt Vertical) unplacedWords rnd n)
        | otherwise = tryToPlace (State placedLetters unplacedWords rnd n) word t
    where
        canPlace direction = checkPlacement placedLetters word (startCell direction) direction n
        startCell direction = getStartCell idx (row, col) direction
        placeIt direction = addToPlaced placedLetters (startCell direction) word direction

-- Checks whether placing the given word at the given starting point will conflict with any existing words
checkPlacement :: [PlacedLetter] -> [Char] -> (Int, Int) -> Direction -> Int -> Bool
checkPlacement placedLetters word startCell direction n =
    doesTheWordFit placedLetters word startCell direction n &&
    canEachLetterBePlaced placedLetters word startCell direction n True

-- Add all the letters in the given word to the board
-- Word will start at the startCell and be placed in the given orientation
-- This makes no checks, so make sure that this is a good placement before calling it!
addToPlaced :: [PlacedLetter] -> (Int, Int) -> [Char] -> Direction -> [PlacedLetter]
addToPlaced placedLetters cell [] direction = placedLetters
addToPlaced placedLetters cell (letter:restOfWord) direction =
    (PlacedLetter letter cell) : addToPlaced placedLetters
                                            (nextCell cell direction)
                                            restOfWord
                                            direction



-- GETTING ALL POSSIBLE INTERSECTIONS

-- Get all intersections between this word and any currently placed words
getAllIntersections :: [Char] -> [PlacedLetter] -> Int -> [Intersection]
getAllIntersections [] placedWords idx = []
getAllIntersections (h:t) placedWords idx =
    (getLetterIntersections h idx placedWords) ++ getAllIntersections t placedWords (idx+1)

-- Get the coordinates of every cell that currently contains the given letter
getLetterIntersections :: Char -> Int -> [PlacedLetter] -> [Intersection]
getLetterIntersections c idx placedLetters =
    map (makeIntersection c idx)
        (filter (\ filledCell -> (getLetter filledCell) == c) placedLetters)
    where
        makeIntersection c idx placedLetter = (Intersection c idx (letterCoordinates placedLetter))



-- PLACEMENT-CHECKING HELPERS

doesTheWordFit :: Foldable t => [PlacedLetter] -> t a -> (Int, Int) -> Direction -> Int -> Bool
doesTheWordFit placedLetters word startCell direction n =
{-- The word fits on the board only if:
    1) There is no letter on the board in front of the first letter
    2) There is no letter on the board after the last letter
    3) Both the first and last letter are in the bounds of the board--}
    (inBounds startCell n && inBounds lastCell n &&
    freeOrOOB (prevCell startCell direction) placedLetters n &&
    freeOrOOB (nextCell lastCell direction) placedLetters n) where
        lastCell = getLastCell startCell direction word

canEachLetterBePlaced :: [PlacedLetter] -> [Char] -> (Int, Int) -> Direction -> Int -> Bool -> Bool
canEachLetterBePlaced placedLetters "" cell direction n _ = True
canEachLetterBePlaced placedLetters (thisLetter:nextLetters) cell direction n wasLastCellEmpty =
    {-- A letter can be placed in a cell only if:
        1) The cell in the bounds of the board
        2) The cell is either
            i) empty AND the "flanking cells" (aka above & below for a horizontal word, left & right for a vertical one) are empty
            ii) OR the cell is not empty but its letter is the same as the given letter --}
    (inBounds cell n) &&
    ((isEmpty cell placedLetters && all (\ cell -> freeOrOOB cell placedLetters n) (flankingCells cell direction))
        || ((getLetterFromCell cell placedLetters) == thisLetter) && wasLastCellEmpty) &&
    canEachLetterBePlaced placedLetters nextLetters (nextCell cell direction) direction n (isEmpty cell placedLetters)

-- Gets the cells above & below this one for Horizontal words, or to the left & right for Vertical words
flankingCells :: (Num a1, Num a2) => (a1, a2) -> Direction -> [(a1, a2)]
flankingCells (row, col) Horizontal = [(row-1, col),(row+1, col)]
flankingCells (row, col) Vertical = [(row, col-1),(row, col+1)]



-- BOARD GENERATION & OUTPUT

-- Makes formatted string of size n*n filled with either placed letters or 'empty cell' characters
makeBoard :: [PlacedLetter] -> Int -> Int -> [Char]
makeBoard placedLetters n i
    | i >= totalCells = []
    | otherwise = space : letter : makeBoard placedLetters n (i+1)
    where
        totalCells = n*n
        (space, letter) = (' ', (getLetterFromCell (i `div` n, i `mod` n) placedLetters))

-- Prints out the board, one row of size n at a time
printBoard :: [Char] -> Int -> IO ()
printBoard "" n = putStrLn("")
printBoard boardString n =
    do
        putStrLn (take (2*n) boardString)
        printBoard (drop (2*n) boardString) n

-- HELPER/UTIL FUNCTIONS

-- get an infinite (lazy) list of random doubles (adapted from Prof. Poole's example)
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
getLetterFromCell :: (Int, Int) -> [PlacedLetter] -> Char
getLetterFromCell cell [] = emptyCellCharacter
getLetterFromCell cell (PlacedLetter letter letterCell :t)
    | cell == letterCell = letter
    | otherwise = getLetterFromCell cell t

-- Check if a cell is within the board
inBounds :: (Ord a, Num a) => (a, a) -> a -> Bool
inBounds (row, col) n = (row>=0) && (row<n) && (col>=0) && (col<n)

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
freeOrOOB :: (Int, Int) -> [PlacedLetter] -> Int -> Bool
freeOrOOB cell placedLetters n = (not (inBounds cell n)) || (isEmpty cell placedLetters)

-- Checks if no letter has been placed has been placed in this cell yet
isEmpty :: (Int, Int) -> [PlacedLetter] -> Bool
isEmpty cell placedLetters = not (any (==cell) (map letterCoordinates placedLetters))





-- GETTERS FOR CUSTOM DATA TYPES

letterCoordinates :: PlacedLetter -> (Int, Int)
letterCoordinates (PlacedLetter letter cell) = cell

getLetter :: PlacedLetter -> Char
getLetter (PlacedLetter letter cell) = letter
