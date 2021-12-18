module Main where

import System.Environment
import Debug.Trace
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "part 1: " ++ (solvePart1 $ head args)
    putStrLn $ "part 2: " ++ (solvePart2 $ head args)

data SFNumber = Literal Int
              | Pair SFNumber SFNumber Int
              | ExplodedPair SFNumber SFNumber Int Int Int
              | Empty
              deriving (Eq)

instance Show SFNumber where
    show (Literal x) = show x
    show (Pair x y d) = "[" ++ show x ++ "," ++ show y ++ "]"--{" ++ show d ++ "}"
    show (ExplodedPair x y a b _) = "Exploded(" ++ show x ++ "," ++ show y ++ "," ++ show a ++ "," ++ show b ++ ")"
    show Empty = "EMPTY"

parseOne :: String -> SFNumber
parseOne x = let foldF (stack1,stack2,prev) char =
                  case char of
                     '[' -> ([[]] ++ stack1,stack2,'[')
                     ']' -> let top1 = head stack1
                             in case (length top1) of
                                   2 -> (tail stack1,(Pair (Literal x) (Literal y) ((length stack1)-1)):stack2,']')
                                       where [x,y] = top1
                                   1 -> let top2 = head stack2
                                         in (tail stack1,(
                                            if prev == ']' 
                                               then Pair (Literal (top1!!0)) top2 ((length stack1)-1)
                                               else Pair top2 (Literal (top1!!0)) ((length stack1)-1)
                                           ):(tail stack2),']')
                                   _ -> (tail stack1,(Pair sndTop2 top2 ((length stack1)-1)):(drop 2 stack2),']')
                                       where top2 = head stack2
                                             sndTop2 = stack2 !! 1
                     ',' -> (stack1,stack2,prev)
                     z -> let top1 = head stack1
                           in ((top1 ++ [ord(z)-48]):(tail stack1),stack2,z)
                 (_,(folded:_),_) = foldl foldF ([],[],'.') x
              in folded

explodeOne :: SFNumber -> (SFNumber, Bool)
explodeOne p@(Pair (Literal x) (Literal y) d) | d >= 4 = (ExplodedPair Empty Empty x y d, True)
explodeOne l@(Literal _) = (l, False)
explodeOne (Pair x y d) = let (explodedX,hasExplodedX) = explodeOne x
                              (explodedY,hasExplodedY) = explodeOne y
                           in case (explodedX,explodedY) of
                                (ExplodedPair Empty Empty a b _,_) -> (ExplodedPair (Literal 0) (addExplosion y b 0) a 0 d, True)
                                (e@(ExplodedPair left right a b _),_) ->
                                    (ExplodedPair (Pair left right (d+1)) (addExplosion y b 0) a 0 d,True)
                                (_,ExplodedPair Empty Empty a b _) -> (ExplodedPair (addExplosion x a 1) (Literal 0) 0 b d, True)
                                (_,e@(ExplodedPair left right a b _)) ->
                                    (ExplodedPair (addExplosion x a 1) (Pair left right (d+1)) 0 b d,True)
                                _ -> (Pair x y d, False)

extractExploded :: SFNumber -> SFNumber
extractExploded (ExplodedPair left right _ _ d) = Pair left right d
extractExploded x = x

addExplosion :: SFNumber -> Int -> Int -> SFNumber
addExplosion (Literal x) y side = Literal (x+y)
--NOTE MISTAKE
--We should take a flag to which side are we adding, rather than
--directly adding it to a, as it could very well need to be added to d
addExplosion (Pair a b d) y side = if side == 0 then Pair (addExplosion a y side) b d
                                                else Pair a (addExplosion b y side) d
addExplosion (ExplodedPair _ _ _ _ _) y _ = error "Can't add explosion to ExplodedPair"
addExplosion Empty y _ = error "Can't add explosion to Empty"

divRoundDown :: Int -> SFNumber
divRoundDown x = Literal (x `div` 2)

divRoundUp :: Int -> SFNumber
divRoundUp x | odd x = Literal ((x `div` 2) + 1)
             | even x = Literal (x `div` 2)

splitOne :: SFNumber -> Int -> (SFNumber, Bool)
splitOne (Literal x) d | x > 9 = (Pair (divRoundDown x) (divRoundUp x) d, True)
splitOne (Pair x y d) _ = let (splitX,hasSplitX) = splitOne x (d+1)
                              (splitY,hasSplitY) = splitOne y (d+1)
                           in case (hasSplitX,hasSplitY) of
                                (True,_) -> (Pair splitX y d, True)
                                (_,True) -> (Pair x splitY d, True)
                                _ -> (Pair x y d, False)
splitOne x d = (x,False)

addTwo :: SFNumber -> SFNumber -> SFNumber
addTwo x y = (Pair (incrementDepth x) (incrementDepth y) 0)

incrementDepth :: SFNumber -> SFNumber
incrementDepth (Pair x y d) = Pair (incrementDepth x) (incrementDepth y) (d+1)
incrementDepth x = x

reduce :: SFNumber -> SFNumber
reduce x = let (exploded,hasExploded) = explodeOne x
               (split,hasSplit) = splitOne x 0
            in case (hasExploded,hasSplit) of
                 (True,_) -> reduce $ extractExploded exploded
                 (_,True) -> reduce split
                 _ -> x

mag :: SFNumber -> Int
mag (Literal x) = x
mag (Pair a b _) = (3*(mag a)) + (2*mag( b))

solvePart1 :: String -> String
solvePart1 input = let inputLines = lines input
                       parsed = map parseOne inputLines
                       added = foldl1 (\a b -> addTwo (reduce a) (reduce b)) parsed
                    in show $ mag $ reduce added

solvePart2 :: String -> String
solvePart2 input = let inputLines = lines input
                       parsed = map parseOne inputLines
                       combos = concat $ map (\x -> map (\y -> (x,y)) $ filter (/=x) parsed) parsed
                    in show $ maximum $ map (\(x,y) -> mag $ reduce $ addTwo x y) combos
