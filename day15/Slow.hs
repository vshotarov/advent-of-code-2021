module Main where

import System.Environment
import Data.Char
import qualified Data.Map as Map
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

toInt :: Char -> Int
toInt x = (ord x) - 48

type Grid = Map.Map (Int,Int) Int
type Point = (Int,Int)

neighbours :: Point -> [Point]
neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

parseInput :: String -> (Grid, Int)
parseInput input = let asLists = map (map toInt) $ lines input
                       w = length asLists
                       asTuples = map (\y -> map (\x -> ((x,y), (asLists !! y) !! x))
                                      [0..w-1]) [0..w-1]
                    in (Map.fromList $ concat asTuples, w)

myLookup :: Point -> Grid -> Int
myLookup (x,y) m = case (Map.lookup (x,y) m) of
                     Nothing -> error ("Looking up " ++ show x ++ ", " ++ show y)
                     Just n -> n

bfs :: [Point] -> Grid -> Grid -> Int -> Grid
bfs [] _ costs _ = costs
bfs (current:explore) grid costs w =
    let isValid (tx,ty) = (tx>=0)&&(tx<w*5)&&(ty>=0)&&(ty<w*5)
        getRisk (x,y) = let baseVal = myLookup (x `mod` w,y `mod` w) grid
                            totalVal = baseVal + (x `div` w) + (y `div` w)
                         in if totalVal <= 9 then totalVal else totalVal `mod` 9
        getCost (x,y) = case Map.lookup (x,y) costs of
                          Nothing -> 1000000
                          Just cost -> cost
        ns = filter isValid $ neighbours current
        nsToUpdate = filter (\n -> (getCost current + getRisk n) < getCost n) ns
        newCosts = foldl (\acc (x,y) -> Map.insert (x,y) (getCost current + getRisk (x,y)) acc)
                         costs nsToUpdate
     in trace (show $ length newCosts) (bfs (explore++nsToUpdate) grid newCosts w)

getLowestCost :: Grid -> Int -> Int
getLowestCost grid w = let costs = Map.singleton (0,0) 0
                           updatedCosts = bfs [(0,0)] grid costs w
                        in myLookup (w*5-1,w*5-1) updatedCosts

solvePart1 :: String -> String
solvePart1 input = let (grid,gridSize) = parseInput input
                    in show $ getLowestCost grid gridSize

solvePart2 :: String -> String
solvePart2 input = "Not implemented"

