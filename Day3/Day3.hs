module Day3.Day3 where

import TestT

import Data.List (transpose, nub, sort)

tests = [ Test {name="Day 3 Part 1 Test"  , input="day3_test.txt"  , subject=part1, assert=Just "198"}
        , Test {name="Day 3 Part 1 Actual", input="day3_actual.txt", subject=part1, assert=Nothing}
        , Test {name="Day 3 Part 2 Test"  , input="day3_test.txt"  , subject=part2, assert=Just "230"}
        , Test {name="Day 3 Part 2 Actual", input="day3_actual.txt", subject=part2, assert=Nothing}
        ]

parse :: String -> [[Int]]
parse = map (map (\x-> read [x] :: Int)) . lines

numsToCols = transpose

occurs :: Int -> [Int] -> Int
occurs x xs = length . filter (==x) $ xs

occurences :: [Int] -> [(Int, Int)]
occurences xs = sort $ zip (map (\x -> occurs x xs)  (nub xs)) (nub xs)

occ = map occurences . transpose

gamma :: [[Int]] -> [Int]
gamma = map (snd . head) . occ

epsilon =  map (snd . last) . occ

bin2decimal :: [Int] -> Int
bin2decimal xs = b2d (reverse xs) 0

b2d :: [Int] -> Int -> Int
b2d [] n = 0
b2d (x:xs) n = (x * 2^n) + (b2d xs (n+1))

part1 :: String -> String
part1 x = show $  (bin2decimal $ gamma xs) * (bin2decimal $ epsilon xs)
        where xs = parse x

---

-- Oxygen generator: Take the gamma, go number-by number until only one candidate remains.
oxygen xs n = if length candidates > 1
                 then oxygen candidates (n+1)
                 else head candidates
        where g = gamma xs
              candidates = filter (\x -> (g !! n) /= (x !! n)) xs

co2 xs n = if length candidates > 1
                 then co2 candidates (n+1)
                 else head candidates
        where g = epsilon xs
              candidates = filter (\x -> (g !! n) /= (x !! n)) xs

part2 :: String -> String
part2 x = show $ ox * co
        where xs = parse x
              ox = bin2decimal $ oxygen xs 0
              co = bin2decimal $ co2 xs 0
