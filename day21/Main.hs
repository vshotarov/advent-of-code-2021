module Main where

import System.Environment
import Debug.Trace
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

solvePart1 :: String -> String
solvePart1 input = let (p1,p2) = (10,8)
                       roll1 d = (d*3)+3
                       roll2 d = ((d+3)*3)+3
                       stepOne f (p,s,d) = let p' = ((p + (f d) -1) `mod` 10) + 1
                                            in (p',s+p',if ((d+6)<=100) then (d+6) else ((d+6) `mod` 100))
                       turns1 = takeWhile (\(_,s,_) -> s<1000) $ iterate (stepOne roll1) (p1,0,1)
                       turns2 = takeWhile (\(_,s,_) -> s<1000) $ iterate (stepOne roll2) (p2,0,1)
                       winTurns = (min (length turns1) (length turns2))
                       ((_,v,_),i) = if (length turns1) > (length turns2)
                                        then ((turns1 !! (winTurns)),(winTurns)*6)
                                        else ((turns2 !! (winTurns-1)),((winTurns-1)*6)+3)
                    in show $ (v * i)

type State = (Integer,Integer,Integer,Integer)
type RollCount = (Integer,Integer)

diceRolls = [x+y+z | x <- [1..3], y <- [1..3], z <- [1..3]]
rollCounts = map (\x -> (x,toInteger $ length $ filter (==x) diceRolls))
                 [(minimum diceRolls)..(maximum diceRolls)]

type Cache = M.Map State (Integer,Integer)

getOutcomes :: State -> Cache -> ((Integer,Integer), Cache)
getOutcomes (_,_,s1,s2) cache | (s1 >= 21) = ((1,0), cache)
getOutcomes (_,_,s1,s2) cache | (s2 >= 21) = ((0,1), cache)
getOutcomes (p1,p2,s1,s2) cache =
    let step (w1,w2) (roll,count) c = let newP1 = ((p1 + roll -1) `mod` 10) + 1
                                          ((y,x),nc) = cached (p2,newP1,s2,s1+newP1) c
                                       in ((w1+(x*count),w2+(y*count)),nc)
        results = foldl (\((s,c):rem) x -> let (res,nc) = step s x c
                                            in ((res,nc):(s,c):rem)) [((0,0),cache)] rollCounts
        newCache = snd $ head results
     in head results

cached :: State -> Cache -> ((Integer,Integer), Cache)
cached s cache = case M.lookup s cache of
                   Nothing -> let (res,nc) = getOutcomes s cache
                               in (res, M.insert s res nc)
                   Just x -> (x, cache)

solvePart2 :: String -> String
solvePart2 input = let (w1,w2) = fst $ cached (10,8,0,0) M.empty
                    in show $ max w1 w2

