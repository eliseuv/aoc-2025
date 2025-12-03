module Days.Day01 (run) where

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type Input = [Int]

parseInput :: String -> Input
parseInput str =
    case parse (some decimal <* eof) "" str of
        Left bundle -> error (errorBundlePretty bundle)
        Right xs -> xs

-- Part A
partA :: Input -> Int
partA input = undefined

-- Part B
partB :: Input -> Int
partB input = undefined

-- Run all
run :: String -> IO ()
run rawInput = do
    let input = parseInput rawInput
    putStrLn "Part A:"
    print (partA input)

    putStrLn "Part B:"
    print (partB input)
