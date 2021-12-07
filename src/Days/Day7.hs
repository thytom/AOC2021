module Days.Day7 where

import TestT

import Data.List.Split (splitOn)
import Data.List (sort)

tests = [ Test {name="Day 7 Part 1", input="day7_test.txt"  , subject=part1, assert=Just "37"}
        , Test {name="Day 7 Part 2", input="day7_test.txt"  , subject=part2, assert=Just "168"} ]
run   = [ Test {name="Day 7 Part 1", input="day7_actual.txt", subject=part1, assert=Just "339321"}
        , Test {name="Day 7 Part 2", input="day7_actual.txt", subject=part2, assert=Just "95476244"} ]
all = tests ++ run

parse :: String -> [Int]
parse = map (read) . splitOn "," . head . lines 

range :: [Int] -> (Int, Int)
range ns = (minimum ns, maximum ns)

diffs :: [Int] -> Int -> [Int]
diffs ns n = map (abs . (n-)) ns

diffs' :: [Int] -> Int -> [Int]
diffs' ns n = map ((\x->sum [1..x]) . abs . (n-)) ns

median :: [Int] -> Int
median ns = sort ns !! (length ns `div` 2)

int_mean :: [Int] -> (Int, Int)
int_mean ns = (\x -> (ceiling x, floor x)) $ sum ns' / (fromIntegral (length ns))
        where ns' = map (fromIntegral :: Int -> Double) ns

best_position :: ([Int] -> Int -> [Int]) -> [Int] -> Int
best_position diff_f ns = head . sort $ [sum $ diff_f ns n | n <- [mn..mx]]
        where (mn, mx) = range ns

part1 :: String -> String
part1 = show . sum . (\x-> diffs x (median x)) . parse

-- Hacky, but works. Just try each side of the mean to find which is better.
part2 :: String -> String
part2 input = show . head . sort . map sum . (\x-> [diffs' ns lower, diffs' x upper]) $ ns
        where ns = parse input
              (lower, upper) = int_mean ns
