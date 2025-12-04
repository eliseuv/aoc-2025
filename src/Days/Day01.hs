module Days.Day01 (run) where

import Data.Text (Text, pack)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf (printf)

type Parser = Parsec Void Text

-- Turn of the dial
data TurnDial
    = TurnLeft Int
    | TurnRight Int
    deriving (Show, Eq)

-- Maximum value for the dial
dialMax :: Int
dialMax = 100

-- Input is a list of dial turns
type Input = [TurnDial]

turnParser :: Parser TurnDial
turnParser =
    choice
        [ parseL
        , parseR
        ]
  where
    parseL = TurnLeft <$> (char 'L' *> decimal)
    parseR = TurnRight <$> (char 'R' *> decimal)

inputParser :: Parser Input
inputParser = turnParser `sepEndBy` newline

parseInput :: String -> Input
parseInput str =
    let parsedResult = parse inputParser "" (Data.Text.pack str)
     in case parsedResult of
            Left bundle -> error (errorBundlePretty bundle)
            Right moves -> moves

-- Turn dial
turnDial :: Int -> TurnDial -> Int
turnDial pos (TurnLeft n) = (pos - n) `mod` dialMax
turnDial pos (TurnRight n) = (pos + n) `mod` dialMax

-- Turn dial keeping track of the number of time the origin is crossed
turnDial' :: Int -> TurnDial -> (Int, Int)
turnDial' pos (TurnRight n) = (pos + n) `divMod` dialMax
turnDial' 0 (TurnLeft n) =
    let (crossings, new_pos) = (-n) `divMod` dialMax
     in (abs crossings - 1, new_pos)
turnDial' pos (TurnLeft n) =
    let (crossings, new_pos) = (pos - n) `divMod` dialMax
     in (abs crossings + fromEnum (new_pos == 0), new_pos)

-- Part A
-- The password is the number of times the dial lands on the origin
partA :: Input -> Int
partA input = length (filter (== 0) dialPositions)
  where
    startingPosition = 50
    dialPositions = scanl turnDial startingPosition input

-- Part B
-- The password is the number of times the dial crosses the origin
partB :: Input -> Int
partB input = sum (map fst dialPositions)
  where
    startingPosition = 50
    dialPositions = scanl (turnDial' . snd) (0, startingPosition) input

-- Run all
run :: String -> IO ()
run rawInput = do
    let input = parseInput rawInput
    printf "Moves read: %d\n" (length input)
    putStrLn "Part A:"
    print (partA input)

    putStrLn "Part B:"
    print (partB input)
