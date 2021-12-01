module Main where

import System.Environment
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

linesToNumbers :: String -> [Int]
linesToNumbers = map read . lines

toPairs :: [Int] -> [(Int, Int)]
toPairs x = zip x (drop 1 x)

numIncreases :: [(Int, Int)] -> Int
numIncreases = length . filter (\(x,y) -> y > x)

solvePart1 :: String -> String
solvePart1 input = show
                   . numIncreases
                   . toPairs $ linesToNumbers input 

toTriplets :: [Int] -> [[Int]]
toTriplets [] = []
toTriplets [_] = []
toTriplets [_,_] = []
toTriplets (a:b:c:xs) = [[a,b,c]] ++ (toTriplets (b:c:xs))

solvePart2 :: String -> String
solvePart2 input = show
                   . numIncreases
                   . toPairs $ map sum (toTriplets $ linesToNumbers input)

