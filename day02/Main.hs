{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

readInt :: String -> Integer
readInt = read

solvePart1 :: String -> String
solvePart1 input = show $ (fst pos) * (snd pos) where
    pos = foldl (\(x,y) (dir:dist:[]) -> case dir of
                  "forward" -> (x+(readInt dist),y)
                  "down" -> (x,y+(readInt dist))
                  "up" -> (x,y-(readInt dist))
                  _ -> error ("unrecognized instruction " ++ dir))
                (0,0)
            $ map words
            $ lines input

solvePart2 :: String -> String
solvePart2 input = show $ outX * outY where
    (outX,outY,_) = foldl (\(x,y,z) (dir:dist:[]) -> case dir of
                  "forward" -> (x+(readInt dist),y+(readInt dist)*z,z)
                  "down" -> (x,y,z+(readInt dist))
                  "up" -> (x,y,z-(readInt dist))
                  _ -> error ("unrecognized instruction " ++ dir))
                (0,0,0)
            $ map words
            $ lines input

