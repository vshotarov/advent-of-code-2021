module Main where

import System.Environment
import Data.List

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solve $ head args)

solve :: String -> String
solve input = let toInt '.' = 0
                  toInt '>' = 1
                  toInt _ = 2
                  parsed = map (\row -> map toInt row) $ lines input
                  stepEastOne row = let stepBody [] = []
                                        stepBody (1:0:rest) = 0:1:(stepBody rest)
                                        stepBody (a:rest) = a:(stepBody rest)
                                     in if (last row == 1) && (head row == 0)
                                           then (1:tail (init $ stepBody row)) ++ [0]
                                           else stepBody row
                  stepEast = map stepEastOne
                  transposeGrid grid = transpose $ map
                       (\row -> map (\x -> [0,2,1] !! x) row) grid
                  stepSouth = transposeGrid . stepEast . transposeGrid
                  step = stepSouth . stepEast
               in show $ length $ takeWhile (\(a,b) -> a/=b) $
                  iterate (\(grid,prev) -> (step grid,grid)) (parsed,[])
 
