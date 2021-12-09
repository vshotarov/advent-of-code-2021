module Main where

import System.Environment
import Data.List.Split
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       secondParts = map (\x -> (splitOn " | " x) !! 1) inputLines
                       only147or8s = map (\x -> length (filter (\y -> case (length y) of
                                                                           2 -> True
                                                                           3 -> True
                                                                           4 -> True
                                                                           7 -> True
                                                                           _ -> False)
                                                                   (words x)))
                                            secondParts
                    in show $ sum only147or8s

isIn :: Char -> String -> Bool
isIn _ [] = False
isIn needle stack = length (filter (\x -> x == needle) stack) > 0

findLeastCommonChars :: [String] -> String
findLeastCommonChars xs = let allChars = "abcdefg"
                              occurrences = map (\x -> length $ filter (\y -> y == x)
                                                                       (concat xs))
                                                allChars
                              minOcc = minimum occurrences
                           in map (\(x,y) -> x) (filter (\(a,b) -> b == minOcc)
                                                        $ zip allChars occurrences)

checkEqual :: String -> String -> Bool
checkEqual a b = (Set.fromList a) == (Set.fromList b)

toNum :: String -> String -> Int
toNum [a,b,c,d,e,f,g] x = case x of
                          x | checkEqual x [a,b,c,e,f,g] -> 0
                          x | checkEqual x [c,f] -> 1
                          x | checkEqual x [a,c,d,e,g] -> 2
                          x | checkEqual x [a,c,d,f,g] -> 3
                          x | checkEqual x [b,c,d,f] -> 4
                          x | checkEqual x [a,b,d,f,g] -> 5
                          x | checkEqual x [a,b,d,e,f,g] -> 6
                          x | checkEqual x [a,c,f] -> 7
                          x | (length x) == 7 -> 8
                          x | checkEqual x [a,b,c,d,f,g] -> 9
                          _ -> error ("Unrecognized " ++ x)
                          

solveOne :: String -> String
solveOne line = let cf = head $ filter (\x -> length x == 2) $ words line
                    acf = head $ filter (\x -> length x == 3) $ words line
                    bcdf = head $ filter (\x -> length x == 4) $ words line
                    a = head $ filter (\x -> not $ x `elem` bcdf) acf
                    fives = filter (\x -> length x == 5) $ words line
                    be = findLeastCommonChars fives
                    e = head $ filter (\x -> not $ x `elem` bcdf) be
                    b = head $ filter (\x -> x /= e) be
                    d = head $ filter (\x -> not $ x `elem` ([b] ++ cf)) bcdf
                    sixes = filter (\x -> length x == 6) $ words line
                    dce = findLeastCommonChars sixes
                    c = head $ filter (\x -> not $ x `elem` [d,e]) dce
                    f = head $ filter (\x -> x /= c) cf
                    g = head $ filter (\x -> not $ x `elem` [a,b,c,d,e,f]) "abcdefg"
                 in [a,b,c,d,e,f,g]

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       inPairs = map (\x -> (splitOn " | " x)) inputLines
                       firstParts = map (\x -> head x) inPairs
                       secondParts = map (\x -> words $ x !! 1) inPairs
                       mappings = map (\x -> solveOne x) firstParts
                       solutions = map (\(x,y) -> map (\z -> toNum x z) y)
                                       $ zip mappings secondParts
                       solutionsAsInts = map (\[a,b,c,d] -> (a*1000)+(b*100)+(c*10)+d) solutions
                    in show $ sum solutionsAsInts

