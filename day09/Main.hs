module Main where

import System.Environment
import Data.Char
import Data.List

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

readInt :: Char -> Int
readInt x = (ord x) - 48

neighbours :: Int -> Int -> Int -> Int -> [(Int,Int)]
neighbours x y width height = filter (\(x,y) -> (x >= 0) && (x < width)
                                             && (y >= 0) && (y < height))
                                      [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                                      
-- Go through each cell and compare it's value to it's neighbours' values
findLowest :: [[Int]] -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
findLowest heightmap lows (x,y) =
    let width = length $ head (heightmap)
        height = length heightmap
        thisNeighbours = neighbours x y width height
        neighbourValues = map (\(nx,ny) -> (heightmap !! ny) !! nx) thisNeighbours
        isLow = (length (filter (\v -> v <= (heightmap !! y) !! x) neighbourValues)) == 0
        newLows = if isLow then lows ++ [(x,y)] else lows
     in if x == (width-1)
           then (if y == height-1 then lows else findLowest heightmap newLows (0,y+1))
        else findLowest heightmap newLows (x+1,y)
                             
isIn :: (Int,Int) -> [(Int,Int)] -> Bool
isIn _ [] = False
isIn needle (x:stack) = if x == needle then True else isIn needle stack

-- BFS
findBasin :: [[Int]] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
findBasin _ visited [] = visited
findBasin heightmap visited ((x,y):toExplore) =
    let width = length $ head (heightmap)
        height = length heightmap
        thisNeighbours = neighbours x y width height
        validNeighbours = filter (\(nx,ny) -> ((heightmap !! ny) !! nx) /= 9) thisNeighbours
        unvisitedNeighbours = filter (\n -> not $ isIn n visited) validNeighbours
     in if isIn (x,y) visited
           then findBasin heightmap visited toExplore
        else findBasin heightmap ([(x,y)]++visited) (toExplore++unvisitedNeighbours)

solve :: String -> String
solve input = let inputLines = lines input
                  heightmap = map (\x -> map readInt x) inputLines
                  lowest = findLowest heightmap [] (0,0)
                  heightsAtLowest = map (\(x,y) -> (heightmap !! y) !! x) lowest
                  risk = sum $ map (\x -> x+1) heightsAtLowest
                  basins = map (\x -> findBasin heightmap [] [x]) lowest
                  basinSizes = map (\x -> length x) basins
                  sortedBasinSizes = reverse $ sort basinSizes
                  largestBasinsProduct = foldr1 (*) $ take 3 sortedBasinSizes
               in "Part 1: " ++ show risk ++ "\nPart 2: " ++ show largestBasinsProduct

