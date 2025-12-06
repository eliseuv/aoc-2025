module Days.Day02 (run) where

import Data.Text (Text, pack)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

type Parser = Parsec Void Text

-- Product ID range
type Range = (Int, Int)

-- Get product IDs from the range
getValues :: Range -> [Int]
getValues (start, end) = [start .. end]

-- We are given a list of ranges
type Input = [Range]

-- Parse a comma separated list of ranges
rangeParser :: Parser Range
rangeParser = do
    start <- L.decimal
    _ <- char '-'
    end <- L.decimal
    return (start, end)

inputParser :: Parser Input
inputParser = rangeParser `sepEndBy` char ','

parseInput :: String -> Input
parseInput str =
    let parsedResult = parse inputParser "" (Data.Text.pack str)
     in case parsedResult of
            Left bundle -> error (errorBundlePretty bundle)
            Right ranges -> ranges

-- Check if a given product ID is invalid
-- A product ID is invalid if it composed of the same sequence of digits repeated twice
isIdInvalid :: Int -> Bool
isIdInvalid productId
    | numDigits `mod` 2 == 1 = False
    | otherwise = and $ uncurry (zipWith (==)) $ splitAt (numDigits `div` 2) productIdDigits
  where
    productIdDigits = show productId
    numDigits = length productIdDigits

-- Generates a list of all non-trivial divisors of a given integer
divisorsOf :: Int -> [Int]
divisorsOf n = filter (\x -> n `mod` x == 0) [2 .. i_max] >>= (\x -> [x, div n x])
  where
    i_max :: Int
    i_max = floor (sqrt (fromIntegral n :: Float))

-- Split a list into chunks of a given length
chunksOf :: Int -> [a] -> [[a]]
chunksOf = helper []
  where
    helper :: [[a]] -> Int -> [a] -> [[a]]
    helper acc _ [] = acc
    helper acc n ls = helper (take n ls : acc) n (drop n ls)

-- Generate all possible chunks integers splitting for a given list
getChunks :: [a] -> [[[a]]]
getChunks str = map (`chunksOf` str) (1 : divisorsOf (length str))

-- Check if a given product ID is invalid
-- A product ID is invalid if it composed only of some sequence of digits repeated at least two times
isIdInvalid' :: Int -> Bool
isIdInvalid' productId = (productId >= 10) && (any (\chunks -> all (== head chunks) (tail chunks)) . getChunks . show) productId

-- Part A
-- Sum of all invalid ranges
partA :: Input -> Int
partA input = sum $ filter isIdInvalid $ input >>= getValues

-- Part B
partB :: Input -> Int
partB input = sum $ filter isIdInvalid' $ input >>= getValues

-- Run all
run :: String -> IO ()
run rawInput = do
    let input = parseInput rawInput
    printf "Ranges read: %d\n" (length input)
    putStrLn "Part A:"
    print (partA input)

    putStrLn "Part B:"
    print (partB input)
