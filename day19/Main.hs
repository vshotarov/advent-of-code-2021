module Main where

import System.Environment
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as Set

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solveBoth $ head args

type Vec3 = (Int,Int,Int)
data Scanner = AlignedScanner (Set.Set Vec3) Vec3
             | MisalignedScanner (Set.Set Vec3)
               deriving (Show,Eq,Ord)
type Grid = M.Map Int Scanner

parse :: String -> Grid
parse xs = let inputLines = lines xs
               scannerBlocks = map tail $ splitOn [""] inputLines
               mapBeacon beacon = let [x,y,z] = splitOn "," beacon
                                   in (read x, read y, read z)
               asGrid = M.fromList $ zip [0..] (map (\scanner ->
                MisalignedScanner (Set.fromList $ map mapBeacon scanner)) scannerBlocks)
            in M.insert 0 (AlignedScanner (Set.fromList $ map mapBeacon $ head scannerBlocks) (0,0,0))
                        asGrid

cross :: Vec3 -> Vec3 -> Vec3
cross (a0,a1,a2) (b0,b1,b2) = ((a1*b2) - (a2*b1),
                               (a2*b0) - (a0*b2),
                               (a0*b1) - (a1*b0))

dot :: Vec3 -> Vec3 -> Int
dot (a1,a2,a3) (b1,b2,b3) = a1*b1 + a2*b2 + a3*b3

rotated :: Int -> Vec3 -> Vec3
rotated rot vec = let forwardUpCombinations = [(0,1),(0, 2),(1,0),(1,2),(2,0),(2,1)] :: [(Int,Int)]
                      signCombinations = [(1,1),(-1,1),(-1,-1),(1,-1)] :: [(Int,Int)]
                      (forward,up) = forwardUpCombinations !! (rot `div` 4)
                      (signF,signU) = signCombinations !! (rot `mod` 4)
                      xaxis = (if forward==0 then signF else 0,
                               if forward==1 then signF else 0,
                               if forward==2 then signF else 0)
                      yaxis = (if up==0 then signU else 0,
                               if up==1 then signU else 0,
                               if up==2 then signU else 0)
                      zaxis = cross xaxis yaxis
                   in (dot xaxis vec, dot yaxis vec, dot zaxis vec)

sub :: Vec3 -> Vec3 -> Vec3
sub (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

add :: Vec3 -> Vec3 -> Vec3
add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

isAligned :: Scanner -> Bool
isAligned (AlignedScanner _ _) = True
isAligned (MisalignedScanner _) = False

getScanner :: Scanner -> Set.Set Vec3
getScanner (AlignedScanner x _) = x
getScanner (MisalignedScanner x) = x

checkOffsets :: Scanner -> Scanner -> [Vec3] -> Maybe Scanner
checkOffsets _ _ [] = Nothing
checkOffsets a b (offset:remainder) =
    let offsetB = Set.map (\x -> sub x offset) (getScanner b)
     in if length (Set.intersection (getScanner a) offsetB) >= 12
           then Just (AlignedScanner offsetB offset)
           else checkOffsets a b remainder

matchScanners :: Scanner -> Scanner -> Int -> Maybe Scanner
matchScanners _ _ 24 = Nothing
matchScanners a b rot =
    let rotatedB = Set.map (rotated rot) (getScanner b)
        offsets = [sub bb ba | ba <- Set.toList (getScanner a),
                               bb <- Set.toList rotatedB]
     in case checkOffsets a (MisalignedScanner rotatedB) offsets of
          Nothing -> matchScanners a b (rot+1)
          x -> x

solve :: Grid -> [Vec3] -> (Grid, [Vec3])
solve grid offsets =
    let misaligned = M.filter (not . isAligned) grid
        matches = M.filter isAligned $
                  M.map (\x -> case matchScanners (grid M.! 0) x 0 of
                                 Nothing -> MisalignedScanner Set.empty
                                 Just y -> y) misaligned
        newOffsets = map (\(_,(AlignedScanner _ x)) -> x) $ M.toList matches
        new0 = AlignedScanner (foldr (\(AlignedScanner x _) acc -> Set.union x acc)
                     (getScanner $ grid M.! 0) matches) (0,0,0)
     in if length misaligned > 0
           then solve (foldr (\x acc -> M.delete x acc) (M.insert 0 new0 grid)
                             (M.keys matches)) (offsets ++ newOffsets)
           else (grid,offsets)

manhattan :: Vec3 -> Vec3 -> Int
manhattan (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs(z1-z2)

solveBoth :: String -> String
solveBoth input = let parsed = parse input
                      (grid,offsets) = solve parsed []
                      matchedBeacons = getScanner $ grid M.! 0
                      longestDistance = maximum [manhattan a b | a <- offsets,
                                                                 b <- offsets]
                   in "Part 1: " ++ show (length matchedBeacons) ++
                    "\nPart 2: " ++ show longestDistance

