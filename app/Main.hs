module Main where

import qualified Days.Day01 as Day01
import qualified Days.Day02 as Day02
import System.Environment (getArgs)
import Text.Printf (printf)

-- | Maps Day number to the specific runner function
dispatch :: Int -> (String -> IO ())
dispatch 1 = Day01.run
dispatch 2 = Day02.run
dispatch n = \_ -> putStrLn $ "Day " ++ show n ++ " not implemented yet."

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dayStr] -> do
            let day = read dayStr :: Int
            let inputFile = printf "data/day%02d.txt" day
            putStrLn $ "Running Day " ++ show day ++ "..."
            input <- readFile inputFile
            dispatch day input
        _ -> putStrLn "Usage: cabal run -- <day_number>"
