module Main where

import System.Environment
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

linesToNumbers :: String -> [Integer]
linesToNumbers = map readInt . lines

toPairs :: [Integer] -> [(Integer, Integer)]
toPairs x = zip x (drop 1 x)

increase :: (Integer, Integer) -> Integer -> Integer
increase (x,y) acc = if y > x then acc + 1 else acc

solvePart1 :: String -> String
solvePart1 input = 
    let numbers = linesToNumbers input
        pairs = toPairs numbers
        numIncreases = foldr increase 0 pairs
        in show numIncreases

solvePart2 :: String -> String
solvePart2 input =
    let numbers = map readInt $ lines input
        triplets = zip (zip numbers (drop 1 numbers)) (drop 2 numbers)
        windows = map (\((x,y),z) -> x+y+z) triplets
        windowPairs = zip windows (drop 1 windows)
        numIncreases = foldr increase 0 windowPairs
        in show numIncreases

readInt :: String -> Integer
readInt = read
