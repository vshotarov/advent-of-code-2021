module Main where

import System.Environment
import Data.List.Split
import qualified Data.Set as Set

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

foldPaper :: [(Int,Int)] -> (Char,Int) -> [(Int,Int)]
foldPaper points (axis,level) = let axisF = if axis == 'x' then fst else snd
                                    below = filter ((level>) . axisF) points
                                    above = filter ((level<) . axisF)  points
                                    maxInAxis = maximum $ map axisF points
                                    offset = (maxInAxis-level)-level
                                    foldX = (map (\(x,y) -> (x+offset,y)) below) ++
                                            (map (\(x,y) -> (2*level-x,y)) above)
                                    foldY = (map (\(x,y) -> (x,y+offset)) below) ++
                                            (map (\(x,y) -> (x,2*level-y)) above)
                                 in if axis == 'x' then foldX else foldY

draw :: [(Int,Int)] -> String
draw points = let (xs,ys) = unzip points
                  (minX,maxX) = (minimum xs, maximum xs)
                  (minY,maxY) = (minimum ys, maximum ys)
                  mapRow = (\y -> map (\x -> if (x,y) `elem` points
                                                then '#' else '.') [minX..maxX])
           in "\n" ++ (concat $ map (\x -> x ++ "\n") $ map mapRow [minY..maxY])

solve :: String -> String
solve input = let inputLines = lines input
                  [numbers,rawFolds] = splitOn [""] inputLines
                  coords = map (\row -> let [x,y] = splitOn "," row
                                         in (read x, read y)) numbers
                  folds = map (\x -> (x!!11, (read $ (splitOn "=" x)!!1))) rawFolds
                  pt1 = length . Set.fromList $ foldPaper coords $ head folds
                  pt2 = foldl foldPaper coords folds
               in "Part 1: " ++ show pt1 ++ "\nPart 2: " ++ (draw $ pt2)

