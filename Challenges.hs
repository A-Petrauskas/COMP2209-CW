{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random


-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- END OF CODE YOU MUST NOT MODIFY


-- Challenge 1 --

-- Adds every word's (String) position and orientation to the result list
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch [] _ = []
solveWordSearch (x:xs) grid =
    checkOrient (startingLetterPos (head x) (0,0) grid) x grid ++ solveWordSearch xs grid


-- Finds all the positions of the starting (first) letter of the given word (String) inside the WordSearchGrid
startingLetterPos :: Char -> Posn -> WordSearchGrid -> [Posn]
startingLetterPos c (a,b) (y:ys) = 
    if (c `elem` y)
        then (addEveryOcc (elemIndices c y) (a,b)) ++ startingLetterPos c (-1,(b+1)) ys
        else startingLetterPos c (-1,(b+1)) ys
startingLetterPos _ _ [] = []

-- Takes all letter's positions in a single row as a list and
-- returns them as Posn according to which row is given (snd Posn)
addEveryOcc :: [Int] -> Posn -> [Posn]
addEveryOcc [] _ = []
addEveryOcc (x:xs) y = [(x, snd (y))] ++ (addEveryOcc xs y)

-- Takes all possible word starting positions and
-- checks every orientation until all positions have been tested.
-- Returns either Nothing or a single position and orientation
checkOrient :: [Posn] -> String -> WordSearchGrid -> [ (String,Maybe Placement) ]
checkOrient [] word grid = [(word, Nothing)]
checkOrient (x:xs) word grid
    | (cForward x (tail word) grid 1) = [(word, Just(x,Forward))]
    | (cBack x (tail word) grid 1) = [(word, Just(x,Back))]
    | (cUp x (tail word) grid 1) = [(word, Just(x,Up))]
    | (cDown x (tail word) grid 1) = [(word, Just(x,Down))]
    | (cUpForward x (tail word) grid 1) = [(word, Just(x,UpForward))]
    | (cUpBack x (tail word) grid 1) = [(word, Just(x,UpBack))]
    | (cDownForward x (tail word) grid 1) = [(word, Just(x,DownForward))]
    | (cDownBack x (tail word) grid 1) = [(word, Just(x,DownBack))]
    | otherwise = checkOrient xs word grid


------All the orientation checks (c) defined below------

cForward :: Posn -> String -> WordSearchGrid -> Int -> Bool
cForward _ [] _ _ = True
cForward x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if ((length (grid !! 0 ) - (fst x)) >= ((length (y:ys)) + 1))
        then
            if ( (((grid !! (snd x)) !! ((fst x) + acc)) == y) )
                then cForward x ys grid (acc + 1)
                else False
        else False

cBack :: Posn -> String -> WordSearchGrid -> Int -> Bool
cBack _ [] _ _ = True
cBack x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if ((length (y:ys) + 1) <= ((fst x) + 1))
        then
            if ( (((grid !! (snd x)) !! ((fst x) - acc)) == y) )
                then cBack x ys grid (acc + 1)
                else False
        else False

cUp :: Posn -> String -> WordSearchGrid -> Int -> Bool
cUp _ [] _ _ = True
cUp x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if ((length (y:ys) + 1) <= ((snd x) + 1))
        then
            if ( (((grid !! ((snd x) - acc)) !! fst x) == y) )
                then cUp x ys grid (acc + 1)
                else False
        else False

cDown :: Posn -> String -> WordSearchGrid -> Int -> Bool
cDown _ [] _ _ = True
cDown x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if ((length (grid !! 0 ) - (snd x)) >= ((length (y:ys)) + 1))
        then
            if ( (((grid !! ((snd x) + acc)) !! fst x) == y) )
                then cDown x ys grid (acc + 1)
                else False
        else False

cUpForward :: Posn -> String -> WordSearchGrid -> Int -> Bool
cUpForward _ [] _ _ = True
cUpForward x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if ((length (y:ys) + 1) <= ((snd x) + 1) 
        && (length (grid !! 0 ) - (fst x)) >= ((length (y:ys)) + 1))
        then
            if ( (((grid !! ((snd x) - acc)) !! ((fst x) + acc)) == y) )
                then cUpForward x ys grid (acc + 1)
                else False
        else False

cUpBack :: Posn -> String -> WordSearchGrid -> Int -> Bool
cUpBack _ [] _ _ = True
cUpBack x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if ((length (y:ys) + 1) <= ((snd x) + 1) && (length (y:ys) + 1) <= ((fst x) + 1))
        then
            if ( (((grid !! ((snd x) - acc)) !! ((fst x) - acc)) == y) )
                then cUpBack x ys grid (acc + 1)
                else False
        else False

cDownForward :: Posn -> String -> WordSearchGrid -> Int -> Bool
cDownForward _ [] _ _ = True
cDownForward x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if (((length (grid !! 0 ) - (snd x)) >= ((length (y:ys)) + 1)) 
        && ((length (grid !! 0 ) - (fst x)) >= ((length (y:ys)) + 1)))
        then
            if ( (((grid !! ((snd x) + acc)) !! ((fst x) + acc)) == y) )
                then cDownForward x ys grid (acc + 1)
                else False
        else False

cDownBack :: Posn -> String -> WordSearchGrid -> Int -> Bool
cDownBack _ [] _ _ = True
cDownBack x (y:ys) grid acc =
    -- Check if word fits in the grid from starting position
    if (((length (grid !! 0 ) - (snd x)) >= ((length (y:ys)) + 1)) 
        && ((length (y:ys) + 1) <= ((fst x) + 1)))
        then
            if ( (((grid !! ((snd x) + acc)) !! ((fst x) - acc)) == y) )
                then cDownBack x ys grid (acc + 1)
                else False
        else False



-- Challenge 2 --

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words density =  do
    grid <- fillGrid words density
    fillwHiddenWords words grid []

fillwHiddenWords :: [ String ] -> WordSearchGrid -> [Posn] -> IO WordSearchGrid
fillwHiddenWords (word:words) grid posns = do
    rndP <- randomPlacement (length (grid !! 0))
    if (checkFit rndP grid posns word)
        then fillwHiddenWords words (addHiddenWord word grid (hiddenWordPos rndP word)) (posns ++ (hiddenWordPos rndP word))
        else fillwHiddenWords (word:words) grid posns
fillwHiddenWords [] grid _ = return grid

fillGrid :: [ String ] -> Double -> IO WordSearchGrid
fillGrid words density = do
    let size = (calcGridSize (lengthAllWords words) density 1)
    replicateM size (replicateM size (randomLetter (givenLetters words)))

checkFit :: Placement -> WordSearchGrid -> [Posn] -> String -> Bool
checkFit plac grid [] word = checkFit plac grid [(-1,-1)] word
checkFit (x,Forward) grid usedPoss word = 
    if ( (length (grid !! 0 ) - (fst x)) >= (length word) )
        then 
            if (checkOverwritting (hiddenWordPos (x,Forward) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,Back) grid usedPoss word = 
    if ( length word <= (fst x) )
        then
            if (checkOverwritting (hiddenWordPos (x,Back) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,Up) grid usedPoss word =
    if ( length word <= (snd x) )
        then 
            if (checkOverwritting (hiddenWordPos (x,Up) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,Down) grid usedPoss word = 
    if ( (length (grid !! 0 ) - (snd x)) >= (length word) )
        then 
            if (checkOverwritting (hiddenWordPos (x,Down) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,UpForward) grid usedPoss word = 
    if (length word <= (snd x) 
        && (length (grid !! 0 ) - (fst x)) >= (length word) )
        then 
            if (checkOverwritting (hiddenWordPos (x,UpForward) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,UpBack) grid usedPoss word = 
    if ( length word <= (snd x) && length word <= (fst x) )
        then 
            if (checkOverwritting (hiddenWordPos (x,UpBack) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,DownForward) grid usedPoss word = 
    if (((length (grid !! 0 ) - (snd x)) >= (length word)) 
        && ((length (grid !! 0 ) - (fst x)) >= (length word)))
        then 
            if (checkOverwritting (hiddenWordPos (x,DownForward) word) word usedPoss grid)
                then True
                else False
        else False
checkFit (x,DownBack) grid usedPoss word = 
    if (((length (grid !! 0 ) - (snd x)) >= (length word)) 
        && (length word <= (fst x)))
        then 
            if (checkOverwritting (hiddenWordPos (x,DownBack) word) word usedPoss grid)
                then True
                else False
        else False


checkOverwritting :: [Posn] -> String -> [Posn] -> WordSearchGrid -> Bool
checkOverwritting [] [] _ _ = True
checkOverwritting (x:xs) (y:ys) usedPos grid =
    if (x `elem` usedPos)
        then 
            if ( ((grid !! (snd x)) !! (fst x)) == y )
                then checkOverwritting xs ys usedPos grid
                else False
        else checkOverwritting xs ys usedPos grid

hiddenWordPos :: Placement -> String -> [Posn]
hiddenWordPos _ [] = []
hiddenWordPos ((a,b),Forward) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a + 1,b),Forward)) xs)
hiddenWordPos ((a,b),Back) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a - 1,b),Back)) xs)
hiddenWordPos ((a,b),Up) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a,b - 1),Up)) xs)
hiddenWordPos ((a,b),Down) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a,b + 1),Down)) xs)
hiddenWordPos ((a,b),UpForward) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a + 1,b - 1),UpForward)) xs)
hiddenWordPos ((a,b),UpBack) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a - 1,b - 1),UpBack)) xs)
hiddenWordPos ((a,b),DownForward) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a + 1,b + 1),DownForward)) xs)
hiddenWordPos ((a,b),DownBack) (x:xs) = [(a,b)] ++ (hiddenWordPos (((a - 1,b + 1),DownBack)) xs)

addHiddenWord :: String -> WordSearchGrid -> [Posn] -> WordSearchGrid
addHiddenWord _ grid [] = grid
addHiddenWord (x:xs) grid (y:ys) = addHiddenWord xs ( replaceRow (snd y) (replaceLetter (fst y) x (grid !! (snd y))) grid ) ys

replaceLetter :: Int -> Char -> [Char] -> [Char]
replaceLetter col letter row = a ++ (letter:b) 
    where (a, (_:b)) = splitAt col row

replaceRow :: Int -> [Char] -> WordSearchGrid -> WordSearchGrid
replaceRow rowN row grid = a ++ (row:b) 
    where (a, (_:b)) = splitAt rowN grid


randomPlacement :: Int -> IO Placement
randomPlacement size = do
    let orients = [Forward,Back,Up,Down,UpForward,UpBack,DownForward,DownBack]
    rnd'1 <- randomRIO (0, size - 1)
    rnd'2 <- randomRIO (0, size - 1)
    rnd'3 <- randomRIO (0,7)
    return ((rnd'1,rnd'2),(orients !! rnd'3))

randomLetter :: String -> IO Char
randomLetter letters = do
    rnd <- randomRIO (0, length letters -1)
    return (letters !! fromIntegral (rnd))

calcGridSize :: Int -> Double -> Int -> Int
calcGridSize wordsL density size = 
    if (((fromIntegral wordsL) / fromIntegral (size * size)) < density)
        then size
        else calcGridSize wordsL density (size + 1)

givenLetters :: [ String ] -> String
givenLetters words = nub $ concat words

lengthAllWords :: [ String ] -> Int
lengthAllWords words = length $ concat words 



-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint _ = ""

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 


-- Challenge 4 --

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro _ = Nothing 


-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
exId =  LamAbs 1 (LamVar 1)
ex5'1 = (LamApp (LamVar 1) (LamVar 2))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Examples in the instructions

-- (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1) (\x2 -> x2)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 