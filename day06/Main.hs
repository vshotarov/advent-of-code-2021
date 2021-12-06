module Main where

import System.Environment
import Data.List.Split

main :: IO ()
main = do
    args <- getArgs
    let counts = histogram $ map read $ splitOn "," $ head args
    putStrLn $ "part 1: " ++ show (sum $ iterate reproduce counts !! 80)
    putStrLn $ "part 2: " ++ show (sum $ iterate reproduce counts !! 256)

-- convert a list of numbers to a histogram, which in
-- our case represents the num of fish currently on day X
histogram :: [Int] -> [Int]
histogram numbers = map (\x -> length $ filter (==x) numbers) [0..8]

-- taking in a histogram return a new one for the next day
reproduce :: [Int] -> [Int]
reproduce [zeros,ones,twos,threes,fours,fives,sixes,sevens,eights] =
    [ones,twos,threes,fours,fives,sixes,sevens+zeros,eights,zeros]
reproduce _ = error "we only accept a list with 9 elements"

