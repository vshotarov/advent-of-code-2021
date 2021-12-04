module Main where

import System.Environment
import Data.List.Split
import Data.List

main :: IO ()
main = do
    args <- getArgs
    let (p1, p2) = solve $ head args
    putStrLn $ "part 1: " ++ p1
    putStrLn $ "part 2: " ++ p2

readInt :: String -> Int
readInt = read

type Board = [Int]

toBoards :: [Board] -> [Int] -> [String] -> [Board]
toBoards boards acc [] = boards ++ [acc]
toBoards boards acc (x:xs) = case x of
                        "" -> toBoards (boards ++ [acc]) [] xs
                        _ -> toBoards boards (acc ++ (map read $ words x)) xs

-- we use -1 to denote a marked character, so five -1s
-- mean a full row or column
isWinner :: Board -> Bool
isWinner xs = let rows = map (\i -> take 5 $ drop (i*5) xs) [0..4]
                  cols = transpose rows
                  winningRows = filter (\x -> x == [-1,-1,-1,-1,-1]) rows
                  winningCols = filter (\x -> x == [-1,-1,-1,-1,-1]) cols
               in ((length winningRows) > 0) || ((length winningCols) > 0)

-- substitute marked characters with -1 and keep going
-- through the list of input numbers, until we win
playBingo :: Board -> [Int] -> (Board, Int, Int)
playBingo board (n:ns) =
    let updatedBoard = map (\x -> if x == n then -1 else x) board
        hasWon = isWinner updatedBoard
     in if hasWon then (updatedBoard, n, length ns)
                  else playBingo updatedBoard ns

calcScore :: Board -> Int -> Int
calcScore board n = n * (sum (map (\x -> if x == -1 then 0 else x) board))

solve :: String -> (String, String)
solve input = let inputNums:_:inputBoards = lines input
                  numbers = map readInt $ splitOn "," inputNums
                  boards = toBoards [] [] inputBoards
                  outBoards = map (\board -> playBingo board numbers) boards
                  sortedBySteps = sortBy (\(_,_,a) (_,_,b) -> compare (-a) (-b)) outBoards
                  (firstWinner,firstWinningNum,_) = head sortedBySteps
                  (lastWinner,lastWinningNum,_) = head (reverse sortedBySteps)
               in (show $ calcScore firstWinner firstWinningNum,
                   show $ calcScore lastWinner lastWinningNum)

