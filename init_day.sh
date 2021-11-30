#!/bin/bash

# cabal file template
name="day$(printf "%02d\n" $1)"
main_template="module Main where

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn \$ \"part 1: \" ++ (solvePart1 \$ head args)
    putStrLn \$ \"part 2: \" ++ (solvePart2 \$ head args)

solvePart1 :: String -> String
solvePart1 input = \"Not implemented\"

solvePart2 :: String -> String
solvePart2 input = \"Not implemented\"
"

cabal_template="executable $name
    main-is:          Main.hs
    build-depends:    base ^>=4.14.3.0
                     ,advent-of-code
    hs-source-dirs:   $name
    default-language: Haskell2010
"

mkdir $name &&
echo "Example input:" &&
example_input=$(</dev/stdin) &&
echo "Real input:" &&
real_input=$(</dev/stdin) &&
touch "$name/Main.hs" "$name/example_input.txt" "$name/input.txt" &&
echo "$example_input" > "$name/example_input.txt" &&
echo "$real_input" > "$name/input.txt" &&
echo "$main_template" > "$name/Main.hs" &&
echo "$cabal_template" >> advent-of-code.cabal &&
echo "Initialized $name"

