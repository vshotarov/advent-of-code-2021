module Main where

import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

data Node = Start | End | Lower String | Upper String
    deriving (Eq, Ord, Show)
type Graph = Map.Map Node [Node]
type Path = [Node]

node :: String -> Node
node x | x == "start" = Start
       | x == "end" = End
       | x == map toLower x = Lower x
       | otherwise = Upper x

buildPaths :: Node -> Path -> Graph -> Bool -> [Path]
buildPaths End path _ _ = [path]
buildPaths cave path caveMap pt2 =
    let go Start = []
        go End = [End:path]
        go n@(Lower _)
          | n `notElem` path = buildPaths n (cave:path) caveMap pt2
          | pt2 = buildPaths n (cave:path) caveMap False
          | otherwise = []
        go n@(Upper _) = buildPaths n (cave:path) caveMap pt2
     in concat $ map go (caveMap Map.! cave)

solve :: String -> String
solve input = let corridors = map (splitOn "-") $ lines input
                  caves = Set.toList $ Set.fromList $ concat corridors
                  caveMap = Map.fromList $ caveMapList where
                      caveMapList = zip (map node caves) (map getCorridors caves)
                      getCorridors = (\cave -> [node (if s == cave then d else s) |
                                                corridor@[s,d] <- corridors,
                                                cave `elem` corridor])
                  paths1 = buildPaths Start [] caveMap False
                  paths2 = buildPaths Start [] caveMap True
               in "Part 1: " ++ show (length paths1) ++
                "\nPart 2: " ++ show (length paths2)

