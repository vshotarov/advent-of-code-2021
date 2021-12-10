module Main where

import System.Environment
import Data.List

pairsOpen = [('(',')'),('[',']'),('{','}'),('<','>')]
pairsClose = map (\(k,v) -> (v,k)) pairsOpen
checkerScores = [(')',3),(']',57),('}',1197),('>',25137)]
completeScores = [('(',1),('[',2),('{',3),('<',4)]

unsafeLookup :: Eq a => a -> [(a,b)] -> b
unsafeLookup x y = case (lookup x y) of
                     Nothing -> error "Lookup found nothing"
                     Just val -> val

main :: IO ()
main = do
    args <- getArgs
    let (part1, part2) = solve $ head args
    putStrLn $ "part 1: " ++ show part1
    putStrLn $ "part 2: " ++ show part2

checkOne :: String -> [Char] -> (Int, [Char])
checkOne [] stack = (0, stack)
checkOne (x:xs) stack =
    case x of
      x | x `elem` ['(','[','{','<'] -> checkOne xs (x:stack)
      closeChar -> if (head stack) /= unsafeLookup closeChar pairsClose
                      then (unsafeLookup closeChar checkerScores , [])
                   else checkOne xs (tail stack)

completeOne :: [Char] -> Int
completeOne xs = foldl (\acc x -> (acc * 5) + unsafeLookup x completeScores) 0 xs

solve :: String -> (Int, Int)
solve input = let inputLines = lines input
                  checked = map (\x -> checkOne x []) inputLines
                  valid = map snd $ filter (\(x,stack) -> length stack > 0) checked
                  completed = map completeOne valid
                  middleScore = (sort completed) !! ((length completed) `div` 2)
               in (sum $ map fst checked, middleScore)

