module Main where

import System.Environment
import qualified Data.Set as S
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

data Cuboid = Cuboid { x1 :: Integer, x2 :: Integer,
                       y1 :: Integer, y2 :: Integer,
                       z1 :: Integer, z2 :: Integer }
                       deriving (Eq, Ord)

toCuboid :: [Integer] -> Cuboid
toCuboid [x1,x2,y1,y2,z1,z2] = Cuboid x1 x2 y1 y2 z1 z2
toCuboid _ = error "toCuboid: Can't create Cuboid"

instance Show Cuboid where
    show (Cuboid x1 x2 y1 y2 z1 z2) = "Cuboid (" ++ show (x1,y1,z1) ++ ") (" ++ show (x2,y2,z2) ++ ")"

intersects :: Cuboid -> Cuboid -> Bool
intersects (Cuboid ax1 ax2 ay1 ay2 az1 az2) (Cuboid bx1 bx2 by1 by2 bz1 bz2) = 
    not ((ax2<bx1)||(bx2<ax1)||(ay2<by1)||(by2<ay1)||(az2<bz1)||(bz2<az1))

subtract :: Cuboid -> Cuboid -> S.Set Cuboid
subtract a b | not $ intersects a b = S.singleton a
subtract (Cuboid ax1 ax2 ay1 ay2 az1 az2) (Cuboid bx1 bx2 by1 by2 bz1 bz2) = 
    let (left,right) = ((ax1,bx1-1), (bx2+1,ax2))
        (bottom,top) = ((ay1,by1-1), (by2+1,ay2))
        (front,back) = ((az1,bz1-1), (bz2+1,az2))
        midX = (max ax1 bx1, min ax2 bx2)
        midY = (max ay1 by1, min ay2 by2)
        midZ = (max az1 bz1, min az2 bz2)
        chunks = [(x,y,z) | x <- [left,midX,right],
                            y <- [bottom,midY,top],
                            z <- [front,midZ,back],
                            (x,y,z) /= (midX,midY,midZ),
                            (snd x) - (fst x) >= 0,
                            (snd y) - (fst y) >= 0,
                            (snd z) - (fst z) >= 0]
     in S.fromList $ map (\((x1,x2),(y1,y2),(z1,z2)) ->
                          Cuboid x1 x2 y1 y2 z1 z2) chunks

volume :: Cuboid -> Integer
volume (Cuboid x1 x2 y1 y2 z1 z2) = ((x2-x1)+1)*((y2-y1)+1)*((z2-z1)+1)

getEnabled :: S.Set Cuboid -> [(Cuboid,Bool)] -> Integer
getEnabled cuboids [] = sum $ map volume $ S.toList cuboids
getEnabled cuboids ((thisCuboid,state):remainder) =
    let newCuboids = S.unions $ map (\c -> Main.subtract c thisCuboid)
                              $ S.toList cuboids
     in if state then getEnabled (S.insert thisCuboid newCuboids) remainder
                 else getEnabled newCuboids remainder

parse :: String -> [(Cuboid,Bool)]
parse x = let inputLines = lines x
              toBool s = if s == "on" then True else False
              readInteger x = read x :: Integer
              toRange cs = concat $ map ((map readInteger) . (splitOn "..")
                                                       . last . (splitOn "="))
                                        $ splitOn "," cs
              split1 = map (splitOn " ") inputLines
            in map (\[s,c] -> (toCuboid $ toRange c, toBool s)) split1

solve :: String -> String
solve input = let parsed = parse input
                  inPt1Range (Cuboid x1 x2 y1 y2 z1 z2) =
                      (x1 <= 50) && (x2 >= -50) &&
                      (y1 <= 50) && (y2 >= -50) &&
                      (z1 <= 50) && (z2 >= -50)
                  inputPt1 = filter (\(c,_) -> inPt1Range c) parsed
               in "Part 1: " ++ show (getEnabled S.empty inputPt1) ++
                "\nPart 2: " ++ show (getEnabled S.empty parsed)

