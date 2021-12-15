{-# OPTIONS_GHC -Wall #-}
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

djikstra :: Int -> Grid -> Grid -> [Point] -> Grid
djikstra _ _ cost [] = cost 
djikstra size grid cost queue =
    let ((px,py):remainder) = sortBy (\a b -> compare (mh a) (mh b)) queue
            where mh (x,y) = x+y
        isOnGrid (x,y) = (x>=0)&&(x<(size*5))&&(y>=0)&&(y<(size*5))
        neighbours = filter isOnGrid [(px+1,py),(px-1,py),(px,py+1),(px,py-1)]
        getRisk (x,y) = let base = grid Map.! (x `mod` size, y `mod` size)
                            total = base + (x `div` size) + (y `div` size)
                         in if total <= 9 then total else (total `mod` 9)
        getCost pt = case Map.lookup pt cost of
                       Nothing -> 1000000
                       Just c -> c
        tentativeCost pt = (getCost (px,py)) + (getRisk pt)
        toExplore = filter (\n -> tentativeCost n < getCost n) neighbours
        newCost = foldl (\acc n -> Map.insert n (tentativeCost n) acc)
                        cost toExplore
     in djikstra size grid newCost (remainder++toExplore)

solve :: String -> String
solve input = let (grid,size) = parseInput input
                  cost = djikstra size grid (Map.singleton (0,0) 0) [(0,0)]
               in "Part 1: " ++ show (cost Map.! (size-1,size-1)) ++
                "\nPart 2: " ++ show (cost Map.! ((size*5)-1,(size*5)-1))

