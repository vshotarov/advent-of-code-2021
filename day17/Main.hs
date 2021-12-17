module Main where

import System.Environment
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

type Vector2 = (Int,Int)

parse :: String -> [Vector2]
parse input = let coords = splitOn ", " $ last $ splitOn "target area: " input
                  ranges = map (last . (splitOn "=")) coords
               in map (\x -> let [b,t] = map read $ splitOn ".." x
                              in (b,t)) ranges

solve :: String -> String
solve input = let [(minX,maxX),(minY,maxY)] = parse input
                  isInTargetArea (x,y) = (x>=minX) && (x<=maxX) &&
                                         (y>=minY) && (y<=maxY)
                  stepV ((x,y),(vx,vy)) = ((x+vx, y+vy), (drag vx, vy-1))
                      where drag 0 = 0
                            drag v = v - (v `div` (abs v))
                  isValid (x,y) = (x<=maxX) && (y>=minY)
                  processOne v = let ps = takeWhile isValid
                                          $ map fst
                                          $ iterate stepV ((0,0),v)
                                     hitsTarget = any isInTargetArea ps
                                  in if hitsTarget then maximum $ map snd ps
                                                   else minY -- could be just -INF
                  velocities = [(x,y) | x <- [0..maxX], y <- [minY..(-minY)]]
                  processed = filter (/=minY) $ map processOne velocities
               in "Part 1: " ++ (show $ maximum processed) ++
                "\nPart 2: " ++ (show $ length processed)

