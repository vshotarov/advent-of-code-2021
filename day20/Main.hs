module Main where

import System.Environment
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

type Algorithm = String
type Image = [String]
type Point = (Int,Int)

parse :: String -> (Algorithm, Image)
parse x = let inputLines = lines x
              blocks = splitOn [""] inputLines
              algo = head blocks
              image = last blocks
              concatAlgo = concat $ map (\row -> filter (/='\n') row) algo
              outImage = map (\row -> filter (/='n') row) image
           in (concatAlgo,outImage)

binToDec :: String -> Int
binToDec bin | length bin /= 9 = error "Not 9"
binToDec bin = foldr (\(i,x) acc -> acc + (if x == '#' then 1 else 0) * (2^i)) 0 $ zip [0..] $ reverse bin

getPixel :: Image -> Point -> Int -> Char
getPixel image (x,y) iter = let (sizeX,sizeY) = (length $ head image, length image)
                             in if (y >= sizeY) || (y < 0)
                                   then "#." !! (iter `mod` 2)
                                   else if (x >= sizeX) || (x < 0)
                                           then "#." !! (iter `mod` 2)
                                           else (image !! y) !! x

draw :: Image -> String
draw image = "\n" ++ (concat $ map (\row -> row ++ "\n") image)

enhance :: Image -> Algorithm -> Int -> Image
enhance input algo iter =
    let (sizeX,sizeY) = (length $ head input, length input)
        newPixel y x = let getInput (xx,yy) = getPixel input (xx,yy) iter
                           bin = map getInput [(x-1,y-1),(x,y-1),(x+1,y-1),
                                               (x-1,y), (x,y),(x+1,y),
                                               (x-1,y+1),(x,y+1),(x+1,y+1)]
                        in algo !! (binToDec bin)
     in map (\y -> map (newPixel y)  [(-1)..(sizeX+0)])
            [(-1)..(sizeY+0)]

numLit :: Image -> Int
numLit image = length $ filter (=='#') $ concat image

solvePart1 :: String -> String
solvePart1 input = let (algo,image) = parse input
                       enhanced = enhance image algo 1
                    in show $ numLit $ enhance enhanced algo 2

solvePart2 :: String -> String
solvePart2 input = let (algo,image) = parse input
                       enhancePair (image,iter) = (enhance image algo iter, iter+1)
                    in show $ numLit $ fst $ (iterate enhancePair (image, 1) !! 50)


