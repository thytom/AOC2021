module Day3.Day3 where

import TestT

import Data.List (transpose, nub, sort)

tests = [ 
          Test {name="Day 3 Part 1 Test"  , input="day3_test.txt"  , subject=part1, assert=Just "198"}
        , Test {name="Day 3 Part 1 Actual", input="day3_actual.txt", subject=part1, assert=Just "3242606"}
        , Test {name="Day 3 Part 2 Test"  , input="day3_test.txt"  , subject=part2, assert=Just "230"}
        , Test {name="Day 3 Part 2 Actual", input="day3_actual.txt", subject=part2, assert=Just "4856080"}
        ]

parse :: String -> [[Int]]
parse = map (map (\x-> read [x] :: Int)) . lines

-- Count the number of times an element appears in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- Convert a list into frequencies
occurences :: (Eq a, Ord a) => [a] -> [(Int, a)]
occurences xs = sort $ zip (map (\x -> length . filter (==x) $ xs)  (nub xs)) (nub xs)

-- Order elements in an array by frequency.
frequencies :: (Eq a, Ord a) => [[a]] -> [[(Int, a)]]
frequencies = map occurences . transpose

gamma :: [[Int]] -> [Int]
gamma = map (snd . head) . frequencies

epsilon :: [[Int]] -> [Int]
epsilon = map (snd . last) . frequencies

-- Simple binary int to decimal converter
bin2dec :: [Int] -> Int
bin2dec xs = sum . map (\(val, exp) -> 2^exp * val)
        . reverse $ zip (reverse xs) [0..]

-- Used to calculate oxygen or co2 depending on the function given.
-- Gamma = exygen, epsilon = Co2
p2f [x] _ _ = x
p2f xs n f = p2f candidates (n+1) f
        where g = f xs
              candidates = filter (\x -> (g !! n) /= (x !! n)) xs

part1 :: String -> String
part1 x = show . product . map (\f-> bin2dec (f xs)) $ [gamma, epsilon]
        where xs = parse x

part2 :: String -> String
part2 x = show . product . map (bin2dec . p2f xs 0) $ [gamma, epsilon]
        where xs = parse x
