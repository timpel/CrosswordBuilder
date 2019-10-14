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

-- DATA TYPES AND GETTERS

-- The State contains all currently placed/unplaced words, an infinite list of random doubles, and the size of the board.
data State = State 
                (Map.Map (Int, Int) Char) -- Map of (x,y) coordinates -> the letter in that cell
                [[Char]]                  -- List of words that we haven't been able to place yet
                [Double]                  -- Infinite list of random doubles 
                Int                       -- size of the board
                Int                       -- Builds attempted in order to reach this state

-- Direction of a word on the board can be horizontal or vertical
data Direction = Horizontal
                | Vertical
    deriving Eq

-- an Intersection is a letter, its index in the word being placed, and an x-y pair of coordinates that it intersects
data Intersection = Intersection Char Int (Int, Int)
    deriving Show


-- ENTRY POINT FOR MANUAL INPUT
start :: IO ()
start = buildFromIO getUserInput

-- ENTRY POINT FOR FILE INPUT
-- Usage: startFromFile fileName boardSize numberOfAttempts
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
            let initState = State Map.empty [] randomDubs boardSize 1
            let prevState = State Map.empty inputList randomDubs boardSize 1
            let (State letterMap unplacedWords rndDoubles boardSize buildsAttempted) = build initState inputList [] prevState inputList timesToTry 1

            printBoard (makeBoard letterMap boardSize 0) boardSize
            putStrLn("Failed to place " ++ show (length unplacedWords) ++ " words: " ++ show unplacedWords)

            let totalBuildsRun = if (length unplacedWords) == 0 then buildsAttempted else timesToTry
            putStrLn("This was build #" ++ show buildsAttempted ++ " out of " ++ show totalBuildsRun ++ " total builds.")

-- BUILDING THE CROSSWORD

-- Build a crossword from the given input words
buildFromIO :: IO InputState -> IO ()
buildFromIO input =
    do
        (InputState inputList boardSize timesToTry) <- input
        putStrLn ("Building a puzzle out of " ++ show inputList)

        randomDubs <- makeRandomGen
        let initState = State Map.empty [] randomDubs boardSize 1
        let prevState = State Map.empty inputList randomDubs boardSize 1
        let (State letterMap unplacedWords rndDoubles boardSize buildsAttempted) = build initState inputList [] prevState inputList timesToTry 1

        printBoard (makeBoard letterMap boardSize 0) boardSize
        putStrLn("Failed to place " ++ show (length unplacedWords) ++ " words: " ++ show unplacedWords)

        let totalBuildsRun = if (length unplacedWords) == 0 then buildsAttempted else timesToTry
        putStrLn("This was build #" ++ show buildsAttempted ++ " out of " ++ show totalBuildsRun ++ " total builds.")


{- Build takes (in order):
        - The current state of the board
        - A list of words left to place for this build
        - A list of words that couldn't be placed in previous attempts of this build
        - The best state seen so far (across all previous builds)
        - The full list of words input that we would like to place
        - The numbers of builds left to try
        - The number of build tried so far
    
    It returns either the bestStateSoFar or the state produced in this build attempt -- whichever has fewer unplaced words.
-}
build :: State -> [[Char]] -> [[Char]] -> State -> [[Char]] -> Int -> Int -> State
build state [] prevUnplaced bestStateSoFar initialList toTry triedSoFar
    | unplaced == [] = state
    | (sort unplaced) == (sort prevUnplaced) =
        if (toTry == triedSoFar)
            then
                newBestState
            else
                build (State Map.empty [] rnd n triedSoFar) initialList [] newBestState initialList toTry (triedSoFar + 1)
    | otherwise = build (State letterMap [] rnd n triedSoFar) unplaced unplaced bestStateSoFar initialList toTry triedSoFar
    where
        newBestState = bestOf state bestStateSoFar
        (State letterMap unplaced rnd n tried) = state

build state (h:t) prevUnplaced bestStateSoFar initialList toTry triedSoFar
    | null letterMap = build (placeFirst state h) t prevUnplaced bestStateSoFar initialList toTry triedSoFar
    | otherwise = build (tryToPlace state h intersections) t prevUnplaced bestStateSoFar initialList toTry triedSoFar
    where
        (State letterMap unplaced rnd n tried) = state
        intersections = randomizedList (getAllIntersections h letterMap 0) rnd

-- Given 2 states, return the one with the fewest unplaced words
bestOf :: State -> State -> State
bestOf newState oldState =
    if ((length unplacedWordsNew) < (length unplacedWordsOld))
        then newState
        else oldState
    where
        (State _ unplacedWordsNew _ _ _) = newState
        (State _ unplacedWordsOld _ _ _) = oldState


-- Place the first word, horizontally, in any random place where it'll fit
placeFirst :: State -> [Char] -> State
placeFirst (State letterMap unplacedWords rnd n triedSoFar) word =
    (State newLetterMap [] newRandList n triedSoFar)
    where
        (rand1:rand2:newRandList) = rnd
        randomValidIdx rndDouble str = randIntFromDouble 0 (n - (length str)) rndDouble
        newLetterMap = addToPlaced letterMap (randomValidIdx rand1 "", randomValidIdx rand2 word) word Horizontal


-- TESTING INTERSECTIONS & PLACING WORDS

tryToPlace :: State -> [Char] -> [Intersection] -> State
-- if there are no intersections to work with, add the word to the Unplaced Words list
tryToPlace (State letterMap unplaced rnd n triedSoFar) word [] =
    (State letterMap (word : unplaced) rnd n triedSoFar)

-- Go through the intersections unti we find one we can place this word at, either horizontally or vertically
tryToPlace (State letterMap unplacedWords rnd n triedSoFar) word ((Intersection letter idx (row, col)) : t)
        | canPlace Horizontal = State (placeIt Horizontal) unplacedWords rnd n triedSoFar
        | canPlace Vertical = State (placeIt Vertical) unplacedWords rnd n triedSoFar
        | otherwise = tryToPlace (State letterMap unplacedWords rnd n triedSoFar) word t
    where
        canPlace direction = checkPlacement letterMap word (startCell direction) direction n
        startCell direction = getStartCell idx (row, col) direction
        placeIt direction = addToPlaced letterMap (startCell direction) word direction

-- Checks whether placing the given word at the given starting point will conflict with any existing words
checkPlacement :: (Map.Map (Int, Int) Char) -> [Char] -> (Int, Int) -> Direction -> Int -> Bool
checkPlacement letterMap word startCell direction n =
    doesTheWordFit letterMap word startCell direction n &&
    canEachLetterBePlaced letterMap word startCell direction n True

-- Add all the letters in the given word to the board
-- Word will start at the startCell and be placed in the given orientation
-- This makes no checks, so make sure that this is a good placement before calling it!
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
doesTheWordFit letterMap word startCell direction n =
{-- The word fits on the board only if:
    1) There is no letter on the board in front of the first letter
    2) There is no letter on the board after the last letter
    3) Both the first and last letter are in the bounds of the board--}
    (inBounds startCell n && inBounds lastCell n &&
    freeOrOOB (prevCell startCell direction) letterMap n &&
    freeOrOOB (nextCell lastCell direction) letterMap n) where
        lastCell = getLastCell startCell direction word

canEachLetterBePlaced :: (Map.Map (Int, Int) Char) -> [Char] -> (Int, Int) -> Direction -> Int -> Bool -> Bool
canEachLetterBePlaced letterMap "" cell direction n _ = True
canEachLetterBePlaced letterMap (thisLetter:nextLetters) cell direction n wasLastCellEmpty =
    {-- A letter can be placed in a cell only if:
        1) The cell in the bounds of the board
        2) The cell is either
            i) empty AND the "flanking cells" (aka above & below for a horizontal word, left & right for a vertical one) are empty
            ii) OR the cell is not empty but its letter is the same as the given letter --}
    (inBounds cell n) &&
    ((isEmpty cell letterMap && all (\ cell -> freeOrOOB cell letterMap n) (flankingCells cell direction))
        || ((getLetterFromCell cell letterMap) == thisLetter) && wasLastCellEmpty) &&
    canEachLetterBePlaced letterMap nextLetters (nextCell cell direction) direction n (isEmpty cell letterMap)

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

-- get an infinite (lazy) list of random doubles (adapted from Prof. Poole's example)
makeRandomGen :: IO [Double]
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

-- Needs infinite list of doubles
randomizedList :: [a] -> [Double] -> [a]
randomizedList [] _ = []
randomizedList [e] _ = [e]
randomizedList (h:t) (d:rd) = randomInsert h (randomizedList t rd) d

randomInsert :: a -> [a] -> Double -> [a]
randomInsert e [] d = [e]
randomInsert e l d = before ++ [e] ++ after
  where (before, after) = splitAt (randIntFromDouble 0 (length l) d) l


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
freeOrOOB :: (Int, Int) -> (Map.Map (Int, Int) Char) -> Int -> Bool
freeOrOOB cell letterMap n = (not (inBounds cell n)) || (isEmpty cell letterMap)

-- Checks if no letter has been placed has been placed in this cell yet
isEmpty :: (Int, Int) -> (Map.Map (Int, Int) Char) -> Bool
isEmpty cell letterMap = not (Map.member cell letterMap)