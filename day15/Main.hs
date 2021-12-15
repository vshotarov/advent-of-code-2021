module Main where

import System.Environment
import Data.Char
import qualified Data.Map as Map
import Data.List

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

type Grid = Map.Map (Int,Int) Int
type Point = (Int,Int)

parseInput :: String -> (Grid, Int)
parseInput input = let toInt x = (ord x) - 48
                       asLists = map (map toInt) $ lines input
                       w = length asLists
                       asTuples = map (\y -> map (\x -> ((x,y), (asLists !! y) !! x))
                                      [0..w-1]) [0..w-1]
                    in (Map.fromList $ concat asTuples, w)

djikstra :: [Point] -> Grid -> Grid -> Int -> Int -> Grid
djikstra [] _ costs _ _ = costs
djikstra explore grid costs w m =
    let isValid (tx,ty) = (tx>=0)&&(tx<w*m)&&(ty>=0)&&(ty<w*m)
        manhattan (x,y) (a,b) = (abs (x-a)) + (abs (y-b))
        getRisk (x,y) = let baseVal = grid Map.! (x `mod` w,y `mod` w)
                            totalVal = baseVal + (x `div` w) + (y `div` w)
                         in if totalVal <= 9 then totalVal else totalVal `mod` 9
        getCost (x,y) = case Map.lookup (x,y) costs of
                          Nothing -> 1000000
                          Just cost -> cost
        to = ((w*m)-1,(w*m)-1)
        sorted = sortBy (\a b -> compare (manhattan b to) (manhattan a to)) explore
        current = head sorted
        neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        ns = filter isValid $ neighbours current
        nsToUpdate = filter (\n -> (getCost current + getRisk n) < getCost n) ns
        newCosts = foldl (\acc (x,y) -> Map.insert (x,y) (getCost current + getRisk (x,y)) acc)
                         costs nsToUpdate
     in djikstra ((tail sorted)++nsToUpdate) grid newCosts w m

getLowestCost :: Grid -> Int -> Int -> Int
getLowestCost grid w m = let costs = Map.singleton (0,0) 0
                             updatedCosts = djikstra [(0,0)] grid costs w m
                          in updatedCosts Map.! (w*m-1,w*m-1)

solve :: String -> String
solve input = let (grid,gridSize) = parseInput input
                  pt1 = getLowestCost grid gridSize 1
                  pt2 = getLowestCost grid gridSize 5
               in "Part 1: " ++ (show pt1) ++
                "\nPart 2: " ++ (show pt2)

