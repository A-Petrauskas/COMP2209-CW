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
import Text.Printf (printf)

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

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


-- Takes all letter's positions in a single given row as a list and returns them as Posn
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


------All the orientation checks defined below------

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

-- First creates a grid full of random chars according to density
-- Then inserts the given words
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words density =  do
    grid <- fillGridRnd words density
    fillwHiddenWords words grid []


-- Places words into the grid according to randomized placement
-- And keeps track of the successfully placed words placements for overwritting checks
fillwHiddenWords :: [ String ] -> WordSearchGrid -> [Posn] -> IO WordSearchGrid
fillwHiddenWords (word:words) grid posns = do
    rndP <- randomPlacement (length (grid !! 0))
    if (checkFit rndP grid posns word)
        then fillwHiddenWords words (addHiddenWord word grid (hiddenWordPos rndP word)) (posns ++ (hiddenWordPos rndP word))
        else fillwHiddenWords (word:words) grid posns
fillwHiddenWords [] grid _ = return grid


-- Creates a grid size (according to density) and fills with random letters (from the input list)
fillGridRnd :: [ String ] -> Double -> IO WordSearchGrid
fillGridRnd words density = do
    let size = (calcGridSize (lengthAllWords words) density 1)
    replicateM size (replicateM size (randomLetter (givenLetters words)))


-- Only returns true when the given word and the hidden words inside the grid
-- have a common letter (crossing) or when the word is a subset of another inside the grid
checkOverwritting :: [Posn] -> String -> [Posn] -> WordSearchGrid -> Bool
checkOverwritting [] [] _ _ = True
checkOverwritting (x:xs) (y:ys) usedPos grid =
    if (x `elem` usedPos)
        then 
            if ( ((grid !! (snd x)) !! (fst x)) == y )
                then checkOverwritting xs ys usedPos grid
                else False
        else checkOverwritting xs ys usedPos grid


-- Adds the word to the grid by changing a single letter at a time inside a row
-- Then puts the new row into the grid
addHiddenWord :: String -> WordSearchGrid -> [Posn] -> WordSearchGrid
addHiddenWord _ grid [] = grid
addHiddenWord (x:xs) grid (y:ys) = addHiddenWord xs ( replaceRow (snd y) (replaceLetter (fst y) x (grid !! (snd y))) grid ) ys


-- Replaces a letter inside a row
replaceLetter :: Int -> Char -> [Char] -> [Char]
replaceLetter col letter row = a ++ (letter:b) 
    where (a, (_:b)) = splitAt col row


-- Replaces a row inside the grid
replaceRow :: Int -> [Char] -> WordSearchGrid -> WordSearchGrid
replaceRow rowN row grid = a ++ (row:b) 
    where (a, (_:b)) = splitAt rowN grid


-- Generates a pseudo-random placement (in bounds of grid size)
randomPlacement :: Int -> IO Placement
randomPlacement size = do
    let orients = [Forward,Back,Up,Down,UpForward,UpBack,DownForward,DownBack]
    rnd'1 <- randomRIO (0, size - 1)
    rnd'2 <- randomRIO (0, size - 1)
    rnd'3 <- randomRIO (0,7)
    return ((rnd'1,rnd'2),(orients !! rnd'3))


-- Generates a pseudo-random letter (from the given input letters)
randomLetter :: String -> IO Char
randomLetter letters = do
    rnd <- randomRIO (0, length letters -1)
    return (letters !! fromIntegral (rnd))


-- Calculates the appropriate grid size according to density
calcGridSize :: Int -> Double -> Int -> Int
calcGridSize wordsL density size = 
    if (((fromIntegral wordsL) / fromIntegral (size * size)) < density)
        then size
        else calcGridSize wordsL density (size + 1)


givenLetters :: [ String ] -> String
givenLetters words = nub $ concat words

lengthAllWords :: [ String ] -> Int
lengthAllWords words = length $ concat words 


-- Returns a word's each letters' position inside the grid
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


-- Checks if a word fits inside the grid according to the given (random) placement
-- Also ensures that placing the word does not destroy any other already hidden words
-- But allows for words to cross over each other
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



-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef m l)
    | (findMacro m l) /= "" = (defMacro m l) ++ (findMacro m l)
    | otherwise = (defMacro m l) ++ (convert m l)


-- Defines macro printing format and converts Macro definitions
defMacro :: [ (String , LamExpr) ] -> LamExpr -> String
defMacro [] _ = ""
defMacro (x:xs) l = printf "def %s = %s in %s" (fst x) (convert [] $ snd x) (defMacro xs l)


-- Tries to Find a Macro definition inside the given lambda expression
-- If found returns the Macro name, else returns the empty string
findMacro :: [ (String , LamExpr) ] -> LamExpr -> String
findMacro [] _ = ""
findMacro (m:ms) lamdaE
    | (snd m == lamdaE) = fst m
    | otherwise = findMacro ms lamdaE


-- Ensures every sub-expression of a macro lambda expression has the corrent bracketing
-- Checking for syntactically equal expressions to the ones defined in Macros 
convert :: [ (String , LamExpr) ] -> LamExpr -> String
convert m (LamMacro name) = printf "%s" name

convert m (LamVar int) = printf "x%d" int

-- Abstraction conversion
convert m (LamAbs int l)
    | (findMacro m (LamAbs int l)) /= "" = 
        printf "%s" (findMacro m (LamAbs int l))

    | (findMacro m l) /= "" = printf "\\x%d -> %s" int $ findMacro m l
    | otherwise = printf "\\x%d -> %s" int (convert m l)

-- Bracketing and conversion for App ( App ( Abs ) )
convert m (LamApp (LamApp (LamAbs int l'1) l'2) l)
    | (findMacro m (LamApp (LamApp (LamAbs int l'1) l'2) l)) /= "" =
        printf "%s" (findMacro m (LamApp (LamApp (LamAbs int l'1) l'2) l))

    | ((findMacro m (LamApp (LamAbs int l'1) l'2)) /= "") && ((findMacro m l) /= "") =
        printf "%s %s" (findMacro m (LamApp (LamAbs int l'1) l'2)) (findMacro m l)
        
    | (findMacro m (LamApp (LamAbs int l'1) l'2)) /= "" =
        printf "%s %s" (convert m (LamApp (LamAbs int l'1) l'2)) (convert m l)

    | (findMacro m l) /= "" = printf "%s %s" (convert m (LamApp (LamAbs int l'1) l'2)) (findMacro m l)
    | otherwise = printf "(%s) %s" (convert m (LamApp (LamAbs int l'1) l'2)) (convert m l)

-- Bracketing and conversion for App ( Abs )
convert m (LamApp (LamAbs int l'1) l)
    | (findMacro m (LamApp (LamAbs int l'1) l)) /= "" =
        printf "%s" (findMacro m (LamApp (LamAbs int l'1) l))

    | ((findMacro m (LamAbs int l'1)) /= "") && ((findMacro m l) /= "") =
        printf "%s %s" (findMacro m (LamAbs int l'1)) (findMacro m l)

    | (findMacro m (LamAbs int l'1)) /= "" = 
        printf "%s %s" (findMacro m (LamAbs int l'1)) (convert m l)

    | (findMacro m l) /= "" = printf "%s %s" (convert m (LamAbs int l'1)) (findMacro m l)
    | otherwise = printf "(%s) %s" (convert m (LamAbs int l'1)) (convert m l)


-- Bracketing and conversion for App ( App )
convert m (LamApp l (LamApp l'1 l'2))
    | findMacro m (LamApp l (LamApp l'1 l'2)) /= "" =
        printf "%s" (findMacro m (LamApp l (LamApp l'1 l'2)))

    | (findMacro m (LamApp l'1 l'2)) /= "" =
        printf "%s %s" (convert m l) (findMacro m (LamApp l'1 l'2))

    | (findMacro m l) /= "" = printf "%s %s" (findMacro m l) (convert m (LamApp l'1 l'2))
    | otherwise = printf "%s (%s)" (convert m l) (convert m (LamApp l'1 l'2))

-- Bracketing and conversion for App 
convert m (LamApp l'1 l'2)
    | ((findMacro m l'1) /= "") && ((findMacro m l'2) /= "") = 
        printf "%s %s" (findMacro m l'1) (findMacro m l'2)

    | (findMacro m l'1) /= "" = printf "%s %s" (findMacro m l'1) (convert m l'2)
    | (findMacro m l'2) /= "" = printf "%s %s" (convert m l'1) (findMacro m l'2)
    | otherwise = printf "%s %s" (convert m l'1) (convert m l'2)



-- Challenge 4 --

-- If the string contains Macro definitions then checks for their uniqueness and
-- and for closed terms. Then procedes with parsing
-- If not - parses
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro string = 
    if (isInfixOf "def" string)
        then if ((null $ parse multipleMacro string) || (null $ parse allParsers afterMacro)
            || (notUniqueMacro (fst $ head $ parse multipleMacro string))
            || (notClosedMacro (map snd (fst $ head $ parse multipleMacro string)))) 

            then Nothing
            else Just $ addMacro (fst $ head $ parse multipleMacro string) (fst $ head $ parse allParsers afterMacro)
        else if (null $ parse allParsers string)
            then Nothing
            else Just $ addMacro [] (fst $ head $ parse allParsers string)
                where afterMacro = snd $ head $ parse (multipleMacro) string
                      allParsers = (appParser <|> absParser <|> getMacroChar)


-- Combines Macro definitions with the Lambda Expression
addMacro :: [ (String,LamExpr) ] -> LamExpr -> LamMacroExpr
addMacro macro expr = (LamDef macro expr)


-- Returns single capital letter (Macro name)
getMacroChar :: Parser LamExpr
getMacroChar = do
    uChar <- upper
    space
    return (LamMacro [uChar])


getVar :: Parser Int
getVar = do
    char 'x'
    num <- nat
    space
    return num

varParser :: Parser LamExpr
varParser = do
    num <- getVar
    space
    return (LamVar num)

brackets :: Parser LamExpr
brackets = do
    space
    char '('
    result <- appParser <|> absParser <|> varParser <|> getMacroChar
    char ')'
    space
    return result


-- Parsers application by parsing the first and second arguments
-- Then checking if its possible to parse further (the sub-expressions)
appParser :: Parser LamExpr
appParser = do
    firstA <- brackets <|> absParser <|> varParser <|> getMacroChar
    secondA <- brackets <|> absParser <|> varParser <|> getMacroChar
    more <- many (appParser <|> absParser <|> varParser <|> getMacroChar)
    if (more == [])
        then return (LamApp firstA secondA)
        else return (LamApp (LamApp firstA secondA) (head more))


-- Parses abstraction by taking the first argument as a LamVar
-- the second argument as a further parse
absParser :: Parser LamExpr
absParser = do
    space
    char '\\'
    lamNum <- getVar
    space
    symbol "->"
    space
    secondA <- appParser <|> absParser <|> brackets <|> varParser <|> getMacroChar
    return (LamAbs lamNum secondA)


-- Parses a single Macro definition
macroParser :: Parser (String,LamExpr)
macroParser = do
    symbol "def"
    space
    name <- upper
    space
    char '='
    space
    macroE <- appParser <|> brackets <|> absParser <|> varParser
    space
    symbol "in"
    space
    return ([name], macroE)


-- Parses multiple Macro definitions
multipleMacro :: Parser [ (String,LamExpr) ]
multipleMacro = do
    result <- some macroParser
    return result


-- Checks whether Macro names repeat
notUniqueMacro :: [ (String,LamExpr) ] -> Bool
notUniqueMacro input =
    if ((length onlyMacroName) /= (length $ nub onlyMacroName))
        then True
        else False
            where onlyMacroName = map fst input


-- Checks whether a Macro body is not closed
notClosedMacro :: [ LamExpr ] -> Bool
notClosedMacro [] = False
notClosedMacro (LamVar _:xs) = True
notClosedMacro (x:xs) = notClosedMacro xs



-- Challenge 5

-- Transforms Macro definitions and the lambda expression seperately and combines them
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef macros lamExpr) =
    addMacro (transformMacros macros allUsed) (transform lamExpr (allUsedNmbrs (show (transformMacros macros allUsed))))
        where allUsed = allUsedNmbrs (show macros) ++ allUsedNmbrs (show lamExpr)


-- Transforms lambda expressions with Macros to cps form
-- Keeps track of used integers
transform :: LamExpr -> [Int] -> LamExpr
-- Macro name transformation
transform (LamMacro x) used = LamMacro x

-- Variable transformation, adds the used integer to the "used" list
transform (LamVar x) used = LamAbs k (LamApp (LamVar k) (LamVar x))
    where k = (freeVarNmbr (used ++ [x]) 1)

-- Abstraction transformation, adds the used integers to the "used" list
transform (LamAbs x y) used = LamAbs k (LamApp (LamVar k) (LamAbs x (transform y (used ++ [x,k]))))
    where k = (freeVarNmbr (used ++ [x]) 1)

-- Abstraction transformation, adds the used integers to the "used" list
transform (LamApp x y) used = LamAbs k ( LamApp (transform x (used ++ [k,f,e])) 
    (LamAbs f (LamApp (transform y (used ++ [k,f,e, (maximum (used ++ [k,f,e]) + 1)])) 
    (LamAbs e (LamApp (LamApp (LamVar f) (LamVar e)) (LamVar k))))))
    where k = (freeVarNmbr used 1)
          f = (freeVarNmbr (used ++ [k]) 1)
          e = (freeVarNmbr (used ++ [k, f]) 1)


-- Returns the next available natural number
freeVarNmbr :: [Int] -> Int -> Int
freeVarNmbr used acc = 
    if (acc `elem` used)
        then freeVarNmbr used (acc + 1)
        else acc


-- Returns a list of all used integers inside the given string (lambda expression with Macros)
allUsedNmbrs :: String -> [Int]
allUsedNmbrs [] = []
allUsedNmbrs (x:xs) 
    | (isDigit x) = [(digitToInt x)] ++ allUsedNmbrs xs
    | otherwise = allUsedNmbrs xs


-- Transforms multiple Macro definitions
transformMacros :: [ (String,LamExpr) ] -> [Int] -> [ (String,LamExpr) ]
transformMacros [] used = []
transformMacros (x:xs) used = 
    [((fst x),(transform (snd x) used))] ++ transformMacros xs (allUsedNmbrs (show(transform (snd x) used)))



-- Challenge 6

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing


-- Checks whether a lambda expression is beta-reducable
canBetaReduce :: LamExpr -> Bool
canBetaReduce (LamApp (LamAbs _ _) _) = True
canBetaReduce (LamApp expr1 expr2) = canBetaReduce expr1 || canBetaReduce expr2
canBetaReduce (LamAbs _ expr) = canBetaReduce expr
canBetaReduce (LamMacro _) = False
canBetaReduce (LamVar _) = False

-- Does a single substitution (beta-reduction)
substitute :: Int -> LamExpr -> LamExpr -> LamExpr
substitute value (LamMacro macro) expr = LamMacro macro

substitute value (LamVar x) expr
    | (x == value) = expr
    | otherwise = LamVar x

substitute value (LamAbs x y) expr
    | (x == value) = (LamAbs x y)
    | otherwise = LamAbs x (substitute value y expr)

substitute value (LamApp x y) expr = LamApp (substitute value x expr) (substitute value y expr)