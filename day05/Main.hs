module Main where

import System.Environment
import Data.List.Split
import Data.List
import Data.Set (fromList)
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       asLineSegs = map asLineSeg inputLines
                       filteredSegs = filter (\((x1,y1),(x2,y2)) -> (x1==x2) || (y1==y2)) asLineSegs
                       intersections = allIntersections filteredSegs
                       uniqueIntersections = fromList intersections
                    in show $ length uniqueIntersections

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       asLineSegs = map asLineSeg inputLines
                       intersections = allIntersections asLineSegs
                       uniqueIntersections = fromList intersections
                    in show $ length uniqueIntersections

type Point = (Int,Int)
type LineSeg = (Point,Point)

asLineSeg :: String -> LineSeg
asLineSeg x = let (inP1:inP2:[]) = splitOn " -> " x
                  p1 = map read $ splitOn "," inP1
                  p2 = map read $ splitOn "," inP2
               in ((p1 !! 0, p1 !! 1), (p2 !! 0, p2 !! 1))

-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
rayIntersect :: LineSeg -> LineSeg -> Maybe Point
rayIntersect (a@(x1,y1),b@(x2,y2)) (c@(x3,y3),d@(x4,y4)) =
    let denominator = ((x1-x2)*(y3-y4))-((y1-y2)*(x3-x4))
        numerator1 = (((x1*y2)-(y1*x2))*(x3-x4))-((x1-x2)*((x3*y4)-(y3*x4)))
        numerator2 = (((x1*y2)-(y1*x2))*(y3-y4))-((y1-y2)*((x3*y4)-(y3*x4)))
     in case denominator of
          0 -> Nothing
          _ -> Just ((numerator1 `div` denominator),(numerator2 `div` denominator))

isOnLine :: Point -> LineSeg -> Bool
isOnLine (x,y) ((x1,y1),(x2,y2)) =
    let dist1 = sqrt $ fromIntegral ((x1-x)^2 + (y1-y)^2)
        dist2 = sqrt $ fromIntegral ((x2-x)^2 + (y2-y)^2)
        dist = sqrt $ fromIntegral ((x2-x1)^2 + (y2-y1)^2)
     in abs (dist - (dist1 + dist2)) < 0.000000001
                       
intersect :: LineSeg -> LineSeg -> Maybe Point
intersect (a@(x1,y1),b@(x2,y2)) (c@(x3,y3),d@(x4,y4)) =
    let rayIntersection = rayIntersect (a,b) (c,d)
     in case rayIntersection of
          Nothing -> Nothing
          Just (x,y) -> let onLineAB = isOnLine (x,y) (a,b)
                            onLineCD = isOnLine (x,y) (c,d)
                         in if onLineAB && onLineCD
                               then Just (x,y)
                            else Nothing

slope :: LineSeg -> Maybe Int
slope ((x1,y1),(x2,y2)) = let xdiff = x2-x1
                              ydiff = y2-y1
                           in case (xdiff,ydiff) of
                                (_, 0) -> Just 0
                                (0, _) -> Nothing
                                _ -> Just (ydiff `div` xdiff)

lineEquation :: LineSeg -> Maybe (Int, Int)
lineEquation (a@(x1,y1),b@(x2,y2)) = let m = slope (a,b)
                                      in case m of
                                           Just theM -> Just (theM, y1-(theM*x1))
                                           Nothing -> Nothing

overlap :: LineSeg -> LineSeg -> [Point]
overlap a@((x1,y1),(x2,y2)) b@((x3,y3),(x4,y4)) =
    let eqA = lineEquation a
        eqB = lineEquation b
        areEqual = case (eqA,eqB) of
                     (Nothing,Nothing) -> x1==x3
                     otherwise -> eqA == eqB
        sortedX = sort [x1,x2,x3,x4]
        sortedY = sort [y1,y2,y3,y4]
     in if areEqual
            then [(x,y) | x <- [(sortedX !! 1)..(sortedX !! 2)],
                          y <- [(sortedY !! 1)..(sortedY !! 2)],
                          (isOnLine (x,y) a) && (isOnLine (x,y) b)]
        else []

checkIntersections :: LineSeg -> [LineSeg] -> [Point]
checkIntersections _ [] = []
checkIntersections line (x:xs) = let overlaps = overlap line x
                                  in case Main.intersect line x of
                                     Nothing -> overlaps ++ (checkIntersections line xs)
                                     Just p -> overlaps ++ [p] ++ (checkIntersections line xs)

allIntersections :: [LineSeg] -> [Point]
allIntersections [] = []
allIntersections (x:xs) = (checkIntersections x xs) ++ (allIntersections xs)

