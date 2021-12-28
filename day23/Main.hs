module Main where

import System.Environment
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

type Cell = (Int,Int)
type Path = S.Set Cell
type MapOfPaths = M.Map Cell Path

hallway = [(x,1) | x <- [1..11]]
hallwayDestinations = filter (\(x,_) -> not $ x `elem` [3,5,7,9]) hallway
rooms1 = map (\x -> map (\y -> (x,y)) [2,3]) [3,5,7,9]
rooms2 = map (\x -> map (\y -> (x,y)) [2,3,4,5]) [3,5,7,9]
allLocations = hallway ++ (concat rooms2)
abcd = M.fromList $ zip "ABCD" [0..]
costs = M.fromList $ zip "ABCD" [1,10,100,1000]

constructPathsFrom :: S.Set (Cell,Int) -> MapOfPaths -> [(Cell,Path)] -> MapOfPaths
constructPathsFrom _ paths [] = paths
constructPathsFrom visited paths (((x,y),path):toExplore) = 
    let distance = length path
        isShorterDistance distance pt =
            case M.lookup pt paths of
              Nothing -> True
              Just existingPath -> distance < (length existingPath)
        inVisited = S.member ((x,y),distance) visited
        neighbours = filter (\pt -> pt `elem` allLocations)
                     [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        toVisit = filter (\pt -> isShorterDistance (distance+1) pt) neighbours
        newVisited = S.insert ((x,y),distance) visited
        newPaths = if isShorterDistance distance (x,y)
                     then M.insert (x,y) (S.insert (x,y) path) paths
                     else paths
        newToExplore = toExplore ++ (map (\pt -> (pt,S.insert (x,y) path)) toVisit)
     in if inVisited then constructPathsFrom visited paths toExplore
                     else constructPathsFrom newVisited newPaths newToExplore

allPaths = M.fromList $ map (\pt -> (pt,
    constructPathsFrom S.empty M.empty [(pt,S.empty)])) allLocations

data Amphipod = Finished Char (Int,Int)
              | Lost Char (Int,Int)
                deriving (Show,Eq,Ord)

getPath :: Cell -> Cell -> Path
getPath from to = (allPaths M.! from) M.! to

parse :: String -> [Amphipod]
parse input = concat $ map (\(y,row) ->
                  foldl (\acc (x,char) -> case M.lookup char abcd of
                                             Nothing -> acc
                                             Just _ -> (Lost char (x,y)):acc)
              [] $ zip [0..] row) $ zip [0..] $ lines input

getPosition :: Amphipod -> (Int,Int)
getPosition (Finished _ x) = x
getPosition (Lost _ x) = x

getLetter :: Amphipod -> Char
getLetter (Finished x _) = x
getLetter (Lost x _) = x

moved :: Amphipod -> (Int,Int) -> Amphipod
moved (Finished _ _) _ = error "Moving a finished amphipod"
moved (Lost a _) pt = Lost a pt

intersects :: Ord a => S.Set a -> S.Set a -> Bool
intersects a b = (length $ S.intersection a b) > 0

firstWhere :: (a -> Bool) -> [a] -> a
firstWhere _ [] = error "firstWhere hit nothing"
firstWhere f (x:xs) = if f x then x
                             else firstWhere f xs

getMoves :: [[Cell]] -> Amphipod -> [Amphipod] -> [(Cell,Int)]
getMoves rooms (Finished _ _) others = []
getMoves rooms (Lost a (x,y)) others =
    let correctRoom = rooms !! (abcd M.! a)
        roomEntrance = (fst $ head correctRoom, 1)
        othersPositions = S.fromList $ map getPosition others
        canReachRoomEntrance = not $ intersects (getPath (x,y) roomEntrance) othersPositions
        isRoomOccupied (Lost b pt) = (pt `elem` correctRoom) && (b /= a)
        isRoomOccupied (Finished _ _) = False
        canGoToRoom = if canReachRoomEntrance then not $ any isRoomOccupied others
                                              else False
        deepestAvailableRoom = firstWhere (\pt -> not $ pt `elem` othersPositions)
                                $ reverse correctRoom
        pathsIfCantGoToRoom = if y == 1 then []
                                        else map (\(pt,pathToPt) -> (pt,(length pathToPt) -1))
                                             $ filter (\(pt,pathToPt) -> not $
                                                intersects pathToPt othersPositions)
                                             $ map (\pt -> (pt, getPath (x,y) pt)) hallwayDestinations
     in if canGoToRoom then [(deepestAvailableRoom,(length $ getPath (x,y) deepestAvailableRoom) -1)]
                       else pathsIfCantGoToRoom

updateOne :: [[Cell]] -> [Amphipod] -> Amphipod -> Amphipod
updateOne rooms pods pod = let (letter,pos) = (getLetter pod, getPosition pod)
                               correctRoom = rooms !! (abcd M.! letter)
                               isInCorrectRoom = pos `elem` correctRoom
                               correctX = fst $ head correctRoom
                               occupiers = map (\pt ->
                                   filter (\p -> (getPosition p) == pt) pods) $ reverse correctRoom
                               check (o:ccupiers) = if (length o == 0) || (getLetter $ head o) /= letter
                                                       then False
                                                       else (if (head o) == pod then True
                                                                                else check ccupiers)
                               finished = check occupiers
                            in if finished then Finished letter pos
                                           else pod

update :: [[Cell]] -> [Amphipod] -> [Amphipod]
update rooms pods = map (updateOne rooms pods) pods

draw :: [Amphipod] -> String
draw pods = let getPodAt pt = let filtered = filter (\p -> getPosition p == pt) pods
                               in if length filtered > 0 then getLetter (head filtered)
                                                         else '.'
             in concat $ map (\y -> (map (\x ->
                    if (x,y) `elem` allLocations then getPodAt (x,y)
                                                 else '#') [0..12]) ++ "\n") [0..6]

solve :: [[Cell]] -> [([Amphipod],Int)] -> S.Set ([Amphipod],Int) -> Int -> Int
solve rooms [] _ bestCost = bestCost
solve rooms ((amphipods,cost):toExplore) visited bestCost =
    let updatedAmphipods = update rooms amphipods
        positions = map getPosition updatedAmphipods
        inVisited = S.member (updatedAmphipods,cost) visited
        isFinished (Finished _ _) = True
        isFinished (Lost _ _) = False
        allFinished = all isFinished updatedAmphipods
        getImportantMoves amph = filter (\(_,newCost) -> newCost < bestCost) $ map (\(pt,dist) ->
                (pt,cost + (dist * (costs M.! (getLetter amph)))))
            $ getMoves rooms amph $ filter (/=amph) updatedAmphipods
        moves = map (\amph -> (amph, getImportantMoves amph)) updatedAmphipods
        performMove amph updatedAmphipods move = [moved amph move] ++ (filter (/=amph) updatedAmphipods)
        newToExplore = (concat $ map (\(amph,amphMoves) ->
                if isFinished amph then []
                                   else map (\(move,moveCost) ->
            (performMove amph updatedAmphipods move,moveCost)) amphMoves) moves) ++ toExplore
        newVisited = S.insert (updatedAmphipods,cost) visited
        (newBestCost,prunedToExplore) = if allFinished && (cost < bestCost)
                                           then (cost, filter (\(s,c) -> c < cost) newToExplore)
                                           else (bestCost,newToExplore)
     in if inVisited then solve rooms toExplore visited bestCost
                     else solve rooms prunedToExplore newVisited newBestCost

solvePart1 :: String -> String
solvePart1 input = let parsed = parse input
                    in show $ solve rooms1 [(parsed,0)] S.empty 1000000

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       newInput = concat $ map (\line -> line ++ "\n") ((take 3 inputLines) ++
                           ["  #D#C#B#A#\n  #D#B#A#C#"] ++ (drop 3 inputLines))
                       parsed = parse newInput
                    in show $ solve rooms2 [(parsed,0)] S.empty 1000000

