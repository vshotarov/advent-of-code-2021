module Main where

import System.Environment
import Data.List
import Data.List.Split

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

exponentialCost :: Int -> [Int] -> Int
exponentialCost x nums = sum $ map (\y -> sum [0..(abs (x-y))]) nums

solvePart1 :: String -> String
solvePart1 input = let numbers = map read $ splitOn "," input
                       distances = map (\x -> sum $ map (\y -> abs (x-y)) numbers)
                                       numbers
                       sortedDistances = sort distances
                    in show $ head sortedDistances

solvePart2 :: String -> String
solvePart2 input = let numbers = map read $ splitOn "," input
                       distances = map (\x -> exponentialCost x numbers)
                                       [0..(maximum numbers)]
                       sortedDistances = sort distances
                    in show $ head sortedDistances
