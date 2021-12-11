module Main where

import System.Environment
import Data.Char
import Data.List.Split

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

readChar :: Char -> Int
readChar x = (ord x) - 48

neighboursAt :: (Int,Int) -> [(Int,Int)]
neighboursAt (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1),
                      (x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]

-- return -1 if out of range, so I can easily filter them
valueAt :: [[Int]] -> (Int,Int) -> Int
valueAt grid (x,y) = let width = length $ head grid
                         height = length grid
                      in if (x<width) && (x>=0) && (y<height) && (y>=0)
                            then (grid !! y) !! x
                         else -1

-- breadth first exploration of newly flashed octopuses
flash :: [[Int]] -> [(Int,Int)] -> [(Int,Int)] -> ([[Int]], [(Int,Int)])
flash grid [] flashed = (grid, flashed)
flash grid ((x,y):exploreList) flashed =
    let neighbours = filter (\(nx,ny) -> (valueAt grid (nx,ny)) /= -1) $ neighboursAt (x,y)
        neighbourPositions = map (\(nx,ny) -> (ny * 10) + nx) neighbours
        newGrid = chunksOf 10 $ map (\(v,i) -> if (i `elem` neighbourPositions)
                                                  then v + 1
                                               else v) $ zip (concat grid) [0..]
        unflashedNeighbours = filter (\x -> not $ x `elem` flashed) neighbours
        newFlashed = filter (\n -> (valueAt newGrid n) > 9) unflashedNeighbours
     in if (x,y) `elem` flashed
           then flash grid exploreList flashed
        else flash newGrid (exploreList ++ newFlashed) (flashed ++ [(x,y)])

-- perform one step -> increase by 1, flash, set flashed to 0
step :: ([[Int]], [(Int,Int)]) -> ([[Int]], [(Int,Int)])
step (grid, _) = let increased = chunksOf 10 $ map (+1) $ concat grid
                     flashed1 = filter (\(x,y) -> (valueAt increased (x,y)) > 9)
                                       [(x,y) | x <- [0..9], y <- [0..9]]
                     (lastGrid, flashed) = flash increased flashed1 []
                     newGrid = chunksOf 10 $ map (\v -> if v > 9 then 0 else v) $ concat lastGrid
                  in (newGrid, flashed)

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       grid = map (\x -> map readChar x) inputLines
                    in show $ snd $ foldr (\_ (thisGrid,total) ->
                        let (newGrid,new) = step (thisGrid,[])
                         in (newGrid,(length new)+total)) (grid,0) [1..100]

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       grid = map (\x -> map readChar x) inputLines
                    in show $ length $ takeWhile (\(_,flashed) -> length flashed /= 100)
                                 $ iterate step (grid,[])

