module Crossword where

import System.IO
import System.Random
import Data.Char
import Data.List

-- OUR MODULES
import CrosswordInput
import CrosswordConstants

-- DATA TYPES AND GETTERS

-- The State contains all currently placed/unplaced words, an infinite list of random doubles, and the size of the board.
data State = State WordPlacements [Double] Int

-- Direction of a word on the board can be horizontal or vertical
data Direction = Horizontal
                | Vertical
    deriving Eq

-- a PlacedLetter consists of a letter and its row/column coordinates, i.e. 'A' (3, 6) == the letter A in cell (3, 6).
data PlacedLetter = PlacedLetter Char (Int, Int)
    deriving Show

-- WordPlacements consists of a list of placed letters and a list of words that couldn't be placed
data WordPlacements = WordPlacements [PlacedLetter] [[Char]]
    deriving Show

-- an Intersection is a letter, its index in the word being placed, and an x-y pair of coordinates that it intersects
data Intersection = Intersection Char Int (Int, Int)
    deriving Show


-- ENTRY POINT!
start :: IO ()
start = buildFromIO getUserInput



-- BUILDING THE CROSSWORD

-- Build a crossword from the given input words
buildFromIO :: IO InputState -> IO ()
buildFromIO input =
    do
        (InputState inputList n) <- input
        putStrLn ("Building a puzzle out of " ++ show inputList)

        randomDubs <- makeRandomGen
        let initState = State (WordPlacements [] []) randomDubs n
        let (State placedWords rndGen n) = build initState inputList []

        printBoard (makeBoard (getPlacedLetters placedWords) n 0) n
        putStrLn("Failed to place these words: " ++ show (getUnplacedWords placedWords))


-- Build takes a state, a list of words to place, and a list of words that were previously unplaced.
build :: State -> [[Char]] -> [[Char]] -> State
build state [] prevUnplaced
    | (sort unplaced) == (sort prevUnplaced) = state
    | otherwise = build (State (WordPlacements placedLetters []) rnd n) unplaced unplaced
    where
        (State (WordPlacements placedLetters unplaced) rnd n) = state
build state (h:t) prevUnplaced
    | null placedLetters = build (placeFirst state h) t prevUnplaced
    | otherwise = build (tryToPlace state h intersections) t prevUnplaced
    where
        (State (WordPlacements placedLetters unplaced) rnd n) = state
        intersections = getAllIntersections h placedLetters 0

-- Place the first word, horizontally, in any random place where it'll fit
placeFirst :: State -> [Char] -> State
placeFirst (State wordPlacements rnd n) word =
    (State (WordPlacements newLetterPlacement []) newRandList n)
    where
        (rand1:rand2:newRandList) = rnd
        randomValidIdx rndDouble str = randIntFromDouble 0 (n - (length str)) rndDouble
        newLetterPlacement = (addToPlaced [] (randomValidIdx rand1 "", randomValidIdx rand2 word) word Horizontal)


-- TESTING INTERSECTIONS & PLACING WORDS

tryToPlace :: State -> [Char] -> [Intersection] -> State
-- if there are no intersections to work with, add the word to the Unplaced Words list
tryToPlace (State (WordPlacements placedLetters unplaced) rnd n) word [] =
    (State (WordPlacements placedLetters (word : unplaced)) rnd n)

-- Go through the intersections unti we find one we can place this word at, either horizontally or vertically
tryToPlace (State wordPlacements rnd n) word ((Intersection letter idx (row, col)) : t)
        | canPlace Horizontal = (State (placeIt Horizontal) rnd n)
        | canPlace Vertical = (State (placeIt Vertical) rnd n)
        | otherwise = tryToPlace (State wordPlacements rnd n) word t
    where
        (WordPlacements placedSoFar cantPlace) = wordPlacements
        canPlace direction = checkPlacement placedSoFar word (startCell direction) direction n
        startCell direction = getStartCell idx (row, col) direction
        placeIt direction = (WordPlacements (addToPlaced placedSoFar (startCell direction) word direction) cantPlace)

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

randIntFromDouble :: Int -> Int -> Double -> Int
randIntFromDouble loInt hiInt rndDouble =
    floor ((hi-lo) * rndDouble + lo)
    where
        hi = fromIntegral hiInt
        lo = fromIntegral loInt

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

getPlacedLetters :: WordPlacements -> [PlacedLetter]
getPlacedLetters (WordPlacements letters _) = letters

getUnplacedWords :: WordPlacements -> [[Char]]
getUnplacedWords (WordPlacements letters unplaced) = unplaced

letterCoordinates :: PlacedLetter -> (Int, Int)
letterCoordinates (PlacedLetter letter cell) = cell

getLetter :: PlacedLetter -> Char
getLetter (PlacedLetter letter cell) = letter