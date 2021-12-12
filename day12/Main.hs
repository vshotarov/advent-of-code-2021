module Main where

import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solvePart1 $ head args

safeLookup :: String -> Map.Map String [String] -> [String]
safeLookup x y = case Map.lookup x y of
                   Nothing -> []
                   Just something -> something

buildPaths :: [String] -> Map.Map String [String] -> String -> [[String]]
buildPaths [] _ _ = error "No path"
buildPaths ("start":x) _ _ = [("start":x)]
buildPaths (node:path) caveMap doubleNode =
    let neighbours = safeLookup node caveMap
        validNeighbours = filter (\x -> if x == map toLower x
                                            then (if x /= doubleNode
                                                     then not $ x `elem` path
                                                   else length (filter (\y -> y==x) path) < 2)
                                        else True) neighbours
        neighbourPaths = map (\x -> buildPaths (x:node:path) caveMap doubleNode) validNeighbours
     in concat neighbourPaths

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       connections = map (\x -> let [src, dst] = splitOn "-" x
                                                 in (src,dst)) inputLines
                       caves = Set.toList $ Set.union (Set.fromList $ map fst connections)
                                                      (Set.fromList $ map snd connections)
                       incoming = map (\x -> map fst $ filter (\(s,d) -> d == x) connections)
                                      caves
                       outgoing = map (\x -> map snd $ filter (\(d,s) -> d == x) connections)
                                      caves
                       caveMap = Map.fromList $ zip caves $ zipWith (++) incoming outgoing
                       paths1 = buildPaths ["end"] caveMap ""
                       paths2 = concat $ map (\x -> if not $ x `elem` ["start","end"]
                                                       then buildPaths ["end"] caveMap x
                                                    else []) caves
                       part2 = length $ Set.toList $ Set.fromList $ paths2 ++ paths1
                    in "Part 1: " ++ show (length paths1) ++ "\nPart 2: " ++ show part2

