module Days.Day03 (run) where

import Data.Text (Text, pack)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, newline)

type Parser = Parsec Void Text

-- A bank is a list of batteries joltage
type Bank = [Int]

digitAsInt :: Parser Int
digitAsInt =
    read . pure <$> digitChar

bankParser :: Parser Bank
bankParser = some digitAsInt

-- A list of battery banks
type Input = [Bank]

inputParser :: Parser Input
inputParser = bankParser `sepEndBy` newline

parseInput :: String -> Input
parseInput str =
    let parsedResult = parse inputParser "" (Data.Text.pack str)
     in case parsedResult of
            Left bundle -> error (errorBundlePretty bundle)
            Right ranges -> ranges

-- Split a list at its maximum value
splitAtMax :: (Ord a) => [a] -> ([a], [a])
splitAtMax [] = ([], [])
splitAtMax xs = break (== maximum xs) xs

-- Calculates the maximum joltage for a given bank using 2 batteries
maximumJoltage :: Bank -> Int
maximumJoltage bank = max lhsJolt rhsJolt
  where
    lhsJolt = if null lhs then 0 else asDecimal (maximum lhs) joltMax
    rhsJolt = if null rhs then 0 else asDecimal joltMax (maximum rhs)

    (lhs, joltMax : rhs) = splitAtMax bank

    asDecimal :: Int -> Int -> Int
    asDecimal d u = read $ show d ++ show u

-- Part A
-- Sum of all maximum bank joltages using 2 batteries
partA :: Input -> Int
partA input = sum $ map maximumJoltage input

-- Part B
partB :: Input -> Void
partB input = undefined

-- Run all
run :: String -> IO ()
run rawInput = do
    let input = parseInput rawInput
    -- print input
    putStrLn "Part A:"
    print (partA input)

    putStrLn "Part B:"
    print (partB input)
