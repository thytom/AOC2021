module Days.Day3 where

import qualified TestT as T
import qualified Days.Day as D

import Data.List (transpose, nub, sort)

day = D.Day { D.name = "Day 3"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day3_test.txt" "198" "230"
            , D.input     = T.mkAssertion "day3_actual.txt" "3242606" "4856080"}

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
