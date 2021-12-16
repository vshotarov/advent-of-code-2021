module Main where

import System.Environment
import Debug.Trace

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ solve $ head args

hexToBin :: String -> String
hexToBin [] = []
hexToBin (x:xs) = let this = case x of
                                '0' -> "0000"
                                '1' -> "0001"
                                '2' -> "0010"
                                '3' -> "0011"
                                '4' -> "0100"
                                '5' -> "0101"
                                '6' -> "0110"
                                '7' -> "0111"
                                '8' -> "1000"
                                '9' -> "1001"
                                'A' -> "1010"
                                'B' -> "1011"
                                'C' -> "1100"
                                'D' -> "1101"
                                'E' -> "1110"
                                'F' -> "1111"
                   in this++(hexToBin xs)

binToDec :: String -> Int
binToDec bin = foldr (\(i,x) acc -> acc + (if x == '1' then 1 else 0) * (2^i)) 0 $ zip [0..] $ reverse bin

data Packet = Literal Int Int
            | Operator Int Int [Packet]
            | Padding
            deriving (Show, Eq)

parseLiteral :: String -> (String, String)
parseLiteral [] = error "Can't parseLiteral empty list"
parseLiteral ('0':x1:x2:x3:x4:xs) = (x1:x2:x3:x4:[], xs)
parseLiteral (x0:x1:x2:x3:x4:xs) = let (next,nextXs) = parseLiteral xs
                                    in (x1:x2:x3:x4:next, nextXs)

parseOperator :: Int -> Int -> String -> (Packet, String)
parseOperator ver _ xs | length (filter (/='0') xs) == 0 = (Padding, "")
parseOperator ver op ('0':xs) = let subpacketsLen = binToDec (take 15 xs)
                                    subpackets = take subpacketsLen (drop 15 xs)
                                    remainder = drop (15 + subpacketsLen) xs
                                 in (Operator ver op (parse [] subpackets), remainder)
parseOperator ver op ('1':xs) = let numSubpackets = binToDec (take 11 xs)
                                    (subpackets,remainder) = (take (numSubpackets+1) $
                                        iterate parseOne ([], drop 11 xs)) !! (numSubpackets)
                                 in (Operator ver op subpackets, remainder)

parseOne :: ([Packet], String) -> ([Packet], String)
parseOne (packets,xs) | length (filter (/='0') xs) == 0 = ([],"")
parseOne (packets,[]) = (packets,[])
parseOne (packets,xs) = let [v,e,r,t,y,p] = take 6 xs
                            ver = binToDec [v,e,r]
                            typ = binToDec [t,y,p]
                            (packet,rem) = case typ of
                                             4 -> let (p,r) = parseLiteral $ drop 6 xs
                                                   in (Literal ver (binToDec p), r)
                                             x -> parseOperator ver x $ drop 6 xs
                         in (packets ++ [packet], rem)

parse :: [Packet] -> String -> [Packet]
parse packets xs | length (filter (/='0') xs) == 0 = packets
parse packets [] = packets
parse packets xs = let (parsed,rem) = parseOne (packets,xs)
                    in parse parsed rem

versionSumOne :: Packet -> Int
versionSumOne (Literal v _) = v
versionSumOne (Operator v _ packets) = v + (sum $ map versionSumOne packets)
versionSumOne Padding = 0

opSum :: Packet -> Int
opSum (Literal _ v) = v
opSum (Operator _ 0 packets) = sum $ map opSum packets
opSum (Operator _ 1 packets) = foldl1 (*) $ map opSum packets
opSum (Operator _ 2 packets) = minimum $ map opSum packets
opSum (Operator _ 3 packets) = maximum $ map opSum packets
opSum (Operator _ 5 [a,b]) = if (opSum a > opSum b) then 1 else 0
opSum (Operator _ 6 [a,b]) = if (opSum a < opSum b) then 1 else 0
opSum (Operator _ 7 [a,b]) = if (opSum a == opSum b) then 1 else 0
opSum (Operator _ x xs) = error ("Unrecognized op " ++ show x ++ ": " ++ show xs)

solve :: String -> String
solve input = let parsed = parse [] $ hexToBin input
                  versionSum = versionSumOne $ head parsed
                  operationSum = opSum $ head parsed
               in "Part 1: " ++ show versionSum ++
                "\nPart 2: " ++ show operationSum

