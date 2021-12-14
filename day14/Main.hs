module Main where

import System.Environment
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

unsafeLookup :: Ord a => a -> Map.Map a b -> b
unsafeLookup x xs = case Map.lookup x xs of
                      Nothing -> error "bad lookup"
                      Just y -> y

type Vector = [Integer]
type Matrix = [Vector]

vecProduct :: Vector -> Vector -> Integer
vecProduct a b = sum $ zipWith (*) a b

vecMatProduct :: Vector -> Matrix -> Vector
vecMatProduct a m = map (\col -> vecProduct a col) $ transpose m

matMatProduct :: Matrix -> Matrix -> Matrix
matMatProduct a b = let nrows = length a
                        flat = [ vecProduct x y | x <- a, y <- transpose b ]
                     in chunksOf nrows flat

getCharCounts :: [String] -> [Integer] -> [Integer]
getCharCounts pairs counts = let pc = zip pairs counts
                                 chars = Set.toList . Set.fromList $
                                         foldl (\acc [x,y] -> x:y:acc) [] pairs
                                 isInPC x (p,c)
                                   | p==[x,x] = 2*c
                                   | x==p!!0= c
                                   | x==p!!1 = c
                                   | otherwise = 0
                                 charCounts = map (\x -> sum $ map (isInPC x) pc) chars
                                 div2 x | odd x = (x `div` 2) + 1
                                 div2 x = x `div` 2
                              in map div2 charCounts

solve :: String -> String
solve input = let inputLines = lines input
                  [[polymer],rawRules] = splitOn [""] inputLines
                  products = map (\x -> let [[a,b],[c]] = splitOn " -> " x
                                         in [a:b:[], a:c:[], c:b:[]]) rawRules
                  -- get all pairs NN, NC, etc.
                  all = Set.toList $ Set.fromList $ concat products
                  -- construct a Map from pair to 2 other pairs
                  rules = Map.fromList $ map (\[x,y,z] -> (x,[y,z])) products
                  num = length all
                  -- construct a matrix of which pair is produced from which other pair
                  flat = [ if (all !! i) `elem` (unsafeLookup (all !! j) rules)
                               then 1 else 0 | i <- [0..num-1], j <- [0..num-1]]
                  -- put the flat matrix from above into a num x num list
                  mat = transpose $ chunksOf num flat
                  -- find all pairs in the starting polymer
                  polymerPairs = map (\(x,y) -> [x,y]) $ zip polymer $ tail polymer
                  -- store their counts
                  pairCounts = map (\x -> toInteger $ length $ filter (==x) polymerPairs) all
                  -- get all the powers of the matrix up to 40
                  matPowers = take 40 $ iterate (matMatProduct mat) mat
                  -- get the pair counts at each step up to 40
                  counts = zipWith vecMatProduct (repeat pairCounts) matPowers
                  solution x = let sorted = sort $ getCharCounts all $ x
                                in (head $ reverse sorted) - (head sorted)
               in "Part 1: " ++ show (solution $ counts !! 9) ++
                "\nPart 2: " ++ show (solution $ counts !! 39)

