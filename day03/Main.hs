module Main where

import System.Environment
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

toInts :: String -> [Int]
toInts xs = map (\x -> if x == '0' then 0 else 1) xs

addPiecewise :: [Int] -> [Int] -> [Int]
addPiecewise x y = map (uncurry (+)) $ zip x y

mostCommon :: [[Int]] -> [Int]
mostCommon [] = error "can't find most common in empty list"
mostCommon xs = let counts = foldr1 (\x acc -> addPiecewise x acc) xs
                    len = length xs
                    halfLen = if (odd len) then (len `div` 2) + 1 else (len `div` 2)
                  in map (\x -> if x >= halfLen then 1 else 0) counts

leastCommon :: [[Int]] -> [Int]
leastCommon [] = error "can't find least common in empty list"
leastCommon xs = let counts = foldr1 (\x acc -> addPiecewise x acc) xs
                     len = length xs
                     halfLen = if (odd len) then (len `div` 2) + 1 else (len `div` 2)
                   in map (\x -> if x >= halfLen then 0 else 1) counts

toDecimal :: [Int] -> Int
toDecimal binary = foldr (\(x,i) acc -> acc + (2^i)*x) 0 $ zip (reverse binary) [0..]

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       numbers = map toInts inputLines
                       gamma = mostCommon numbers
                       epsilon = map (1-) gamma
                     in show $ (toDecimal gamma) * (toDecimal epsilon)

sift :: ([[Int]] -> [Int]) -> [[Int]] -> [Int] -> [Int]
sift _ [] _ = error "can't sift an empty list"
sift _ [x] bits = bits++x
sift f numbers bits = let (bit:_) = f numbers
                          sifted = map (\(x:xs) -> xs) $ filter (\(x:xs) -> x == bit) numbers
                        in sift f sifted (bits++[bit])

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       numbers = map toInts inputLines
                       oxygen = sift mostCommon numbers []
                       co2 = sift leastCommon numbers []
                     in show $ (toDecimal oxygen) * (toDecimal co2)

