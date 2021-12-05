module Main where

import System.Environment
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

asLineSeg :: String -> ((Integer,Integer),(Integer,Integer))
asLineSeg x = let (inP1:inP2:[]) = splitOn " -> " x
                  p1 = map read $ splitOn "," inP1
                  p2 = map read $ splitOn "," inP2
               in ((p1 !! 0, p1 !! 1), (p2 !! 0, p2 !! 1))
                   
find :: (Integer,Integer) -> [(Integer,Integer)] -> Bool
find _ [] = False
find needle (s:tack)
  | needle == s = True
  | otherwise = find needle tack

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

processOne :: ((Integer,Integer),(Integer,Integer)) -> [(Integer,Integer)]
processOne ((x1,y1),(x2,y2)) = let operation = if (x1 == x2) || (y1 == y2)
                                                  then cartProd
                                               else zip
                                in operation (if x1 < x2 then [x1..x2] else reverse [x2..x1])
                                             (if y1 < y2 then [y1..y2] else reverse [y2..y1])

fill :: [(Integer,Integer)]  -> [(Integer,Integer)] -> [((Integer,Integer),(Integer,Integer))] -> [(Integer, Integer)]
fill one moreThanOne [] = moreThanOne
fill one moreThanOne (line:lines) =
    let cells = processOne line
        inOne = filter (\x -> find x one) cells
        notInMoreThanOne = filter (\x -> not $ find x moreThanOne) inOne
     in fill (one ++ cells) (moreThanOne ++ notInMoreThanOne) lines

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       asLineSegs = map asLineSeg inputLines
                       filteredLineSegs = filter (\((x1,y1),(x2,y2)) -> (x1 == x2) || (y1 == y2)) asLineSegs
                    in show . length $ fill [] [] filteredLineSegs

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       asLineSegs = map asLineSeg inputLines
                    in show . length $ fill [] [] asLineSegs

