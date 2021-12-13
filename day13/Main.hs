module Main where

import System.Environment
import Data.List.Split
import qualified Data.Set as Set

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

foldOne :: (Char,Int) -> (Int,Int) -> (Int,Int)
foldOne ('x',level) (x,y) = if x < level then (x,y) else (2*level-x,y)
foldOne (_,level) (x,y) = if y < level then (x,y) else (x,2*level-y)

draw :: [(Int,Int)] -> String
draw points = "\n"++[charAt (x,y) | y <- [minY..maxY], x <- [minX..maxX+1]]
    where (xs, ys) = unzip points
          (minX,maxX) = (minimum xs, maximum xs)
          (minY,maxY) = (minimum ys, maximum ys)
          charAt (x,_) | x==maxX+1 = '\n'
          charAt (x,y) = if (x,y) `elem` points then '#' else '.'

solve :: String -> String
solve input = let inputLines = lines input
                  [numbers,rawFolds] = splitOn [""] inputLines
                  coords = map (\row -> let [x,y] = splitOn "," row
                                         in (read x, read y)) numbers
                  folds = map (\x -> (x!!11, (read $ (splitOn "=" x)!!1))) rawFolds
                  pt1 = length . Set.fromList $ (foldOne $ head folds) <$> coords
                  pt2 = foldl (\acc fold -> (foldOne fold) <$> acc) coords folds
               in "Part 1: " ++ show pt1 ++ "\nPart 2: " ++ (draw $ pt2)

